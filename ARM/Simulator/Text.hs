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
                     ,input :: String
                     ,output :: String
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
       runWorld $ World True bus reset as "" "" 

  runWorld :: World -> IO ()
  runWorld w = if running w 
                then do w' <- performIO w 
                        w'' <- busCycle w'
                        runWorld w''
                else return ()

  performIO :: World -> IO World
  performIO w@(World _ _ _ as input output) =
    do output' <- if null output 
                    then return output
                    else do putStr output; return ""
       asyncResult <- poll as
       case asyncResult of
            Just (Right c) ->
              do as' <- async getChar
                 return w {as = as', input = input ++ [c], output = output'}
            Just (Left e) -> error $ show e
            Nothing -> return w {output = output'}


  busCycle :: World -> IO World
  busCycle w@(World run bus machine _ input output) = do
    let pc = evalState (getRegister R15) machine
    (ir, bus', input', output') <- busRead bus pc WORD input output
    let (action, machine') = 
          flip runState machine $ do modify $ \s -> s {ir = ir}
                                     setRegister R15 (pc + 4)
                                     step
    case action of
      Stop -> return w {running = False, bus = bus', machine = machine'}
      Continue -> return w {bus = bus', machine = machine'}
      ReadMem rd addr sz sg -> 
        do (val, bus'', input'', output'') <- busRead bus' addr sz input' output'
           let bits = case sz of WORD -> 0; HALF -> 16; BYTE -> 24
           let val' = if sg then dropBits bits val
                            else let sVal = fromIntegral val :: Int32 
                                 in fromIntegral $ dropBits bits sVal
           let machine'' = flip execState machine' $ setRegister rd val'
           return w {bus = bus'', machine = machine''
                    ,input = input'', output = output'}
      WriteMem rd addr sz -> 
        do let val = flip evalState machine' $ getRegister rd 
           (bus'', input'', output'') <- busWrite bus' addr sz val input' output'
           return w {bus = bus'', machine = machine'
                    ,input = input'', output = output''}
    where dropBits :: (Bits a) => Int -> a -> a
          dropBits sh val = (val `shiftL` sh) `shiftR` sh

