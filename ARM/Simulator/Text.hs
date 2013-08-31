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

  memSize :: (Integral a) => a
  memSize = 0x200000


  simulate :: [Word32] -> IO ()
  simulate program = 
    do hSetBuffering stdin NoBuffering
       hSetBuffering stdout NoBuffering
       bus <- makeBus program
       busCycles bus reset

  makeBus :: [Word32] -> IO Bus
  makeBus program = 
    do as <- async getChar
       mem <- newListArray (0, memSize `div` 4) $ program ++ repeat 0
       return [makeIODevice $ IODevice 0xFF00 8 as Nothing Nothing
              ,makeMemDevice $ MemoryDevice 0x0 memSize mem
              ]

  data IODevice = IODevice {ioStart :: BusAddress
                           ,ioLen :: BusAddress
                           ,ioAsync :: Async Char
                           ,input :: Maybe Char
                           ,output :: Maybe Char
                           }

  makeIODevice :: IODevice -> BusDevice
  makeIODevice io =
    BusDevice {containsAddr = \addr -> ioStart io <= addr && 
                                        addr < ioStart io + ioLen io
              ,devRead = 
                \addr sz -> 
                  do (val, io') <- ioRead io (addr - ioStart io) sz 
                     return (val, makeIODevice io')
              ,devWrite = 
                \addr sz val -> 
                  liftM makeIODevice $ ioWrite io (addr - ioStart io) sz val 
                  
              }


  ioRead :: IODevice -> BusAddress -> MemSize -> IO (Word32, IODevice)
  ioRead io@(IODevice _ _ as input output) 0 BYTE = 
    case input of
      Nothing -> 
        do asyncResult <- poll as 
           case asyncResult of 
                Just (Right c) -> 
                  do as' <- async getChar
                     return (0xF, io {ioAsync = as', input = Just c})
                Just (Left e) -> error $ show e
                Nothing -> return (0x0A, io)
      Just c -> return $ (0x0F, io)

  ioRead io@(IODevice s l _ Nothing o) 2 BYTE = error $ "No input available"
  ioRead io@(IODevice _ _ _ (Just c) _) 2 BYTE = 
    return (fromIntegral $ fromEnum c, io)

  ioRead io@(IODevice _ _ _ _ _) 0 HALF = ioRead io 0 BYTE
  ioRead io@(IODevice _ _ _ _ _) 2 HALF = ioRead io 2 BYTE

  ioRead io@(IODevice _ _ _ _ _) 0 WORD = do
    (d0, io') <- ioRead io 0 BYTE
    (d2, io'') <- ioRead io' 2 BYTE
    return (d2 `shiftL` 16 .|. d0, io'')

  ioWrite :: IODevice -> BusAddress -> MemSize -> Word32 -> IO IODevice

  ioWrite io@(IODevice start len as input output) 0 BYTE val =
    do output' <- if val .&. 0xA /= 0
                    then case output of 
                              Nothing -> error "No output character"
                              Just c -> do putChar c; return Nothing
                    else return output
       let input' = if val .&. 0x5 /= 0 then Nothing else input
       return $ io { input = input', output = output' }

  ioWrite io@(IODevice _ _ as input _) 2 BYTE val =
    return $ io {output = Just $ toEnum $ fromIntegral val }
  
  ioWrite io@(IODevice _ _ as input _) 4 BYTE val =
    return $ io { output = Just $ toEnum $ fromIntegral val }


  busCycles :: Bus -> Machine -> IO ()
  busCycles bus machine = do
    let pc = evalState (getRegister R15) machine
    (ir, bus') <- busRead bus pc WORD
    let (action, machine') = 
          flip runState machine $ do modify $ \s -> s {ir = ir}
                                     setRegister R15 (pc + 4)
                                     step
    case action of
      Stop -> return ()
      Continue -> busCycles bus' machine'
      ReadMem rd addr sz sg -> 
        do (val, bus'') <- busRead bus' addr sz 
           let bits = case sz of WORD -> 0; HALF -> 16; BYTE -> 24
           let val' = if sg then dropBits bits val
                            else let sVal = fromIntegral val :: Int32 
                                 in fromIntegral $ dropBits bits sVal
           let machine'' = flip execState machine' $ setRegister rd val'
           busCycles bus'' machine''
      WriteMem rd addr sz -> 
        do let val = flip evalState machine' $ getRegister rd 
           bus'' <- busWrite bus' addr sz val 
           busCycles bus'' machine'
    where dropBits :: (Bits a) => Int -> a -> a
          dropBits sh val = (val `shiftL` sh) `shiftR` sh

