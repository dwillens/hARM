module ARM.Simulator.Text (simulate) where
  import ARM.Disassembler
  import ARM.InstructionSet as I
  import ARM.Simulator.Common
  import Control.Concurrent.Async
  import Control.Monad.State
  import Data.Array
  import Data.Bits
  import Data.Int
  import Data.Maybe
  import Data.Array.IO
  import Data.Array.MArray
  import Data.Word
  import System.IO

  import Debug.Trace
  import Text.Printf

  data World = World {running :: Bool
                     ,bus :: Bus
                     ,machine :: Machine
                     ,as :: Async Char
                     ,busIO :: BusIO
                     }

  memSize :: (Integral a) => a
  memSize = 0x200000

  simulate :: [Word32] -> IO ()
  simulate program = 
    do hSetBuffering stdin NoBuffering
       hSetBuffering stdout NoBuffering
       term <- hIsTerminalDevice stdin
       when term $ hSetEcho stdin False
       bus <- makeBus program
       as <- async getChar
       runWorld $ World True bus reset as ("", "")

  runWorld :: World -> IO ()
  runWorld w = if running w 
                then do w' <- performIO w 
                        w'' <- busCycle w'
                        runWorld w''
                else return ()

  performIO :: World -> IO World
  performIO w@(World _ _ _ as (input, output)) =
    do output' <- if null output 
                    then return output
                    else do putStr output; return ""
       asyncResult <- poll as
       case asyncResult of
            Just (Right c) ->
              do as' <- async getChar
                 return w {as = as', busIO = (input ++ [c], output')}
            Just (Left e) -> error $ show e
            Nothing -> return w {busIO = (input, output')}

  worldRead :: BusAddress -> MemSize -> StateT World IO Word32
  worldRead addr sz = do
    bus <- gets bus
    busIO <- gets busIO
    ((val, bus'), busIO') <- lift $ flip runStateT busIO $ busRead bus addr sz
    modify $ \w -> w {bus = bus', busIO = busIO'}
    return val

  worldWrite :: BusAddress -> MemSize -> Word32 -> StateT World IO ()
  worldWrite addr sz val = do
    bus <- gets bus
    busIO <- gets busIO
    (bus', busIO') <- lift $ flip runStateT busIO $ busWrite bus addr sz val
    modify $ \w -> w {bus = bus', busIO = busIO'}

  fetchAndStep :: StateT World IO Action
  fetchAndStep = do
    machine <- gets machine
    let pc = flip evalState machine $ getRegister R15
    ir <- worldRead pc WORD
    let (action, machine') = 
          flip runState machine $ do modify $ \s -> s {ir = ir}
                                     setRegister R15 (pc + 4)
                                     step
    modify $ \w -> w {machine = machine'}
    return action

  execRead :: Register -> BusAddress -> MemSize -> Signedness ->
             StateT World IO ()
  execRead rd addr sz sg =
    do val <- worldRead addr sz
       let bits = case sz of WORD -> 0; HALF -> 16; BYTE -> 24
       let val' = if sg then dropBits bits val
                       else let sVal = fromIntegral val :: Int32 
                            in fromIntegral $ dropBits bits sVal
       machine <- gets machine
       let machine' = flip execState machine $ setRegister rd val'
       modify $ \w -> w {machine = machine'}
       return ()
    where dropBits :: (Bits a) => Int -> a -> a
          dropBits sh val = (val `shiftL` sh) `shiftR` sh

  execWrite :: Register -> BusAddress -> MemSize -> StateT World IO ()
  execWrite rd addr sz = do
    machine <- gets machine
    let val = flip evalState machine $ getRegister rd 
    worldWrite addr sz val

  busCycle :: World -> IO World
  busCycle = execStateT $ do
    action <- fetchAndStep
    case action of
      Stop -> modify $ \w -> w {running = False}
      Continue -> return ()
      ReadMem rd addr sz sg -> execRead rd addr sz sg
      WriteMem rd addr sz -> execWrite rd addr sz

