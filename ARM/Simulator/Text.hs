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
       as <- async getChar
       let bus = makeBus program
       runWorld $ World True bus reset as "" "" 

  runWorld :: World -> IO ()
  runWorld w = if running w 
                then do w' <- performIO w 
                        runWorld $ busCycle w'
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


  busCycle :: World -> World
  busCycle w@(World run bus machine _ input output) =
    let pc = evalState (getRegister R15) machine
        (ir, bus', input', output') = busRead bus pc WORD input output
        (action, machine') = 
          flip runState machine $ do modify $ \s -> s {ir = ir}
                                     setRegister R15 (pc + 4)
                                     step
    in 
    case action of
      Stop -> w {running = False, bus = bus', machine = machine'}
      Continue -> w {bus = bus', machine = machine'}
      ReadMem rd addr sz sg -> 
        let (val, bus'', input'', output'') = busRead bus' addr sz input' output'
            bits = case sz of WORD -> 0; HALF -> 16; BYTE -> 24
            val' = if sg then dropBits bits val
                         else let sVal = fromIntegral val :: Int32 
                              in fromIntegral $ dropBits bits sVal
            machine'' = flip execState machine' $ setRegister rd val'
        in w {bus = bus'', machine = machine'', input = input'', output = output'}
      WriteMem rd addr sz -> 
        let val = flip evalState machine' $ getRegister rd 
            (bus'', input'', output'') = busWrite bus' addr sz val input' output'
        in w {bus = bus'', machine = machine', input = input'', output = output''}
    where dropBits :: (Bits a) => Int -> a -> a
          dropBits sh val = (val `shiftL` sh) `shiftR` sh

