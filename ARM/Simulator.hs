module ARM.Simulator (simulate) where
  import ARM.Disassembler
  import ARM.InstructionSet as I
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

  data Machine = Machine {rf :: Array Register Word32
                         ,ir :: Word32
                         ,z :: Bool
                         ,n :: Bool
                         ,c :: Bool
                         ,v :: Bool
                         }
    deriving (Show)

  memSize :: (Integral a) => a
  memSize = 0x80000

  reset :: Machine
  reset = Machine 
    {rf = listArray (minBound, maxBound) $ repeat 0 
    ,z = False
    ,n = False
    ,c = False
    ,v = False
    ,ir = 0
    }
  
  simulate :: [Word32] -> IO ()
  simulate program = 
    do bus <- createBus program
       busCycles bus reset

  createBus :: [Word32] -> IO Bus
  createBus program = 
    do as <- async getChar
       mem <- newListArray (0, memSize) $ take memSize $ program ++ repeat 0
       return [BusDevice (IODevice as Nothing Nothing) 0xFF00 0xFF08
              ,BusDevice (MemoryDevice mem) 0x0 (memSize * 4)]

  busCycles :: Bus -> Machine -> IO ()
  busCycles bus machine = do
    let pc = evalState (getRegister R15) machine
    (bus', ir) <- busRead bus pc WORD
    let (action, machine') = 
          flip runState machine $ do modify $ \s -> s {ir = ir}
                                     setRegister R15 (pc + 4)
                                     step
    case action of
      Stop -> return ()
      Continue -> busCycles bus' machine'
      ReadMem rd addr sz sg -> 
        do (bus'', val) <- busRead bus' addr sz 
           let sh = case sz of WORD -> 0; HALF -> 16; BYTE -> 24
           let val' = if sg 
                        then (val `shiftL` sh) `shiftR` sh 
                        else let sVal = fromIntegral val :: Int32 
                             in fromIntegral ((sVal `shiftL` sh) `shiftR` sh)
           let machine'' = flip execState machine' $ setRegister rd val'
           busCycles bus'' machine''
      WriteMem rd addr sz -> 
        do let val = flip evalState machine' $ getRegister rd 
           bus' <- busWrite bus addr sz val 
           busCycles bus' machine'

  data Device = IODevice (Async Char) (Maybe Char) (Maybe Char)
              | MemoryDevice (IOArray BusAddress Word32)
  
  data BusDevice = BusDevice {device :: Device
                             ,busStart :: BusAddress
                             ,busEnd :: BusAddress
                             }
  type Bus = [BusDevice]



  readComplete :: Register -> Signedness -> (Int, Int) -> Word32 -> 
                  State Machine ()
  readComplete rd sg (shL, shR) val =
    let sVal :: Int32
        sVal = fromIntegral val
        val' :: Word32
        val' = case sg of
                    False -> (val `shiftL` shL) `shiftR` shR
                    True -> fromIntegral ((sVal `shiftL` shL) `shiftR` shR)
    in setRegister rd val'

  busRead :: Bus -> BusAddress -> MemSize -> IO (Bus, Word32)
  busRead (dev:devs) addr sz
    | addr >= busStart dev && addr < busEnd dev = 
        do (dev', val) <- devRead (device dev) (addr - busStart dev) sz
           return (dev {device = dev'}:devs, val)
    | otherwise = 
        do (devs', val) <- busRead devs addr sz
           return (dev:devs', val)

  busWrite :: Bus -> Word32 -> MemSize -> Word32 -> IO Bus
  busWrite (dev:devs) addr sz val
    | addr >= busStart dev && addr < busEnd dev = 
        do dev' <- devWrite (device dev) (addr - busStart dev) sz val
           return $ dev {device = dev'}:devs
    | otherwise = 
        do devs' <- busWrite devs addr sz val
           return $ dev:devs'

  devRead :: Device -> BusAddress -> MemSize -> IO (Device, Word32)
  devRead io@(IODevice as input output) 0 BYTE = 
    case input of
      Nothing -> 
        do asyncResult <- poll as 
           case asyncResult of 
                Just (Right c) -> do as' <- async getChar
                                     return (IODevice as' (Just c) output, 0x0F)
                Just (Left e) -> error $ show e
                Nothing -> return (io, 0x0A)
      Just c -> return $ (io, 0x0F)

  devRead (IODevice _ Nothing _) 2 BYTE = error $ "No input available"
  devRead io@(IODevice _ (Just c) _) 2 BYTE = 
    return (io, fromIntegral $ fromEnum c)

  devRead io@(IODevice _ _ _) 0 HALF = devRead io 0 BYTE
  devRead io@(IODevice _ _ _) 2 HALF = devRead io 2 BYTE

  devRead io@(IODevice _ _ _) 0 WORD = do
    (io', d0) <- devRead io 0 BYTE
    (io'', d2) <- devRead io' 2 BYTE
    return (io'', d2 `shiftL` 16 .|. d0)

  devRead (MemoryDevice m) addr WORD
    | addr `mod` 4 == 0 = do val <- readArray m (addr `div` 4)
                             return (MemoryDevice m, val)
    | otherwise = error $ "Unaligned word read at " ++ show addr

  devRead (MemoryDevice m) addr BYTE =
    do val <- readArray m (addr `div` 4)
       let shift = fromIntegral $ (addr `mod` 4) * 8
       --traceShow (addr, (printf "%08x" val :: String), shift, (val `shiftR` shift) .&. 0xFF) $ return ()
       return (MemoryDevice m, (val `shiftR` shift) .&. 0xFF)

  devWrite :: Device -> BusAddress -> MemSize -> Word32 -> IO Device
  devWrite io@(IODevice as input output) 0 BYTE val =
    do output' <- if val .&. 0xA /= 0
                    then case output of 
                              Nothing -> error "No output character"
                              Just c -> do putChar c
                                           hFlush stdout
                                           return Nothing
                    else return output
       let input' = if val .&. 0x5 /= 0 then Nothing else input
       return $ IODevice as input' output'

  devWrite io@(IODevice as input _) 2 BYTE val =
    return $ IODevice as input $ Just $ toEnum $ fromIntegral val
  
  devWrite io@(IODevice as input _) 4 BYTE val =
    return $ IODevice as input $ Just $ toEnum $ fromIntegral val

  devWrite (MemoryDevice m) addr WORD val
    | addr `mod` 4 == 0 = do writeArray m (addr `div` 4) val
                             return $ MemoryDevice m
    | otherwise = error $ "Unaligned word write at " ++ show addr

  devWrite (MemoryDevice m) addr BYTE val =
    do old <- readArray m (addr `div` 4)
       let shift = flip shiftL $ fromIntegral $ (addr `mod` 4) * 8
       let mask = shift 0xFF
       let new = (old .&. complement mask) .|. (shift val .&. mask)
       writeArray m (addr `div` 4) new
       return $ MemoryDevice m

  data Action = Stop
              | Continue
              | ReadMem Register BusAddress MemSize Signedness
              | WriteMem Register BusAddress MemSize

  step :: State Machine Action
  step = do   
    i <- liftM disassembleI $ gets ir
    (c, n, v, z) <- getFlags
    --rf <- gets rf
    --traceShow (i, c, n, v, z, rf) $ return ()
    if not $ condition i c n v z 
      then return Continue 
      else do a <- stepI i
              pc <- getRegister R15 
              return $ if pc == 0 then Stop else a

  stepI :: Instruction -> State Machine Action
  stepI (DP op _ s rd rn so) =
    do op1 <- getRegister rn
       (op2, sc) <- getShifterOperand so
       (c, z, n, v) <- getFlags
       let ci = fromIntegral $ fromEnum c
       let (wb, result, c', v') = alu op op1 op2 ci sc v
       let n' = result > maxBound `div` 2
       let z' = result == 0
       when s $ setFlags (c', n', v', z')
       when wb $ setRegister rd result
       return Continue

  stepI (B _ lnk offsetStr) =
    do let offset = (read offsetStr :: Int32) `shiftL` 8 `shiftR` 8
       oldPC <- getRegister R15
       if lnk then setRegister R14 oldPC else return () 
       setRegister R15 $ oldPC + fromIntegral offset
       return Continue

  stepI (MEM op _ sg sz rd rn dir (MEMI offset)) =
    do base <- getRegister rn
       let addr = base + fromIntegral offset
       case op of 
        LDR -> return $ ReadMem rd addr sz sg 
        STR -> return $ WriteMem rd addr sz

  stepI i = error $ show i

  condition :: Instruction -> Bool -> Bool -> Bool -> Bool -> Bool
  condition (DP _ cc _ _ _ _) = condition' cc
  condition (B cc _ _) = condition' cc
  condition (MEM _ cc _ _ _ _ _ _) = condition' cc

  condition' :: ConditionCode -> Bool -> Bool -> Bool -> Bool -> Bool
  condition' I.EQ _ _ _ z = z
  condition' I.NE _ _ _ z = not z
  condition'   CS c _ _ _ = c
  condition'   CC c _ _ _ = not c
  condition'   MI _ n _ _ = n
  condition'   PL _ n _ _ = not n
  condition'   VS _ _ v _ = v
  condition'   VC _ _ v _ = not v
  condition'   HI c _ _ z = c && not z
  condition'   LS c _ _ z = not c || z
  condition'   GE _ n v _ = n == v
  condition' I.LT _ n v _ = n /= v
  condition' I.GT _ n v z = n == v && not z
  condition'   LE _ n v z = n /= v || z
  condition'   AL _ _ _ _ = True

  alu :: DPOpcode -> Word32 -> Word32 -> Word32 -> Bool -> Bool -> 
              (Bool, Word32, Bool, Bool)
  alu AND a b c sc v = (True, a .&. b, sc, v)
  alu EOR a b c sc v = (True, a `xor` b, sc, v)
  alu SUB a b c _  _ = (True, a - b, not $ carry a (-b) 0, overflow a (-b) 0)
  alu RSB a b c _  _ = (True, b - a, not $ carry (-a) b 0, overflow (-a) b 0)
  alu ADD a b c _  _ = (True, a + b, carry a b 0, overflow a b 0)
  alu ADC a b c _  _ = (True, a + b + c, carry a b c, overflow a b c)
  alu SBC a b c _  _ = let nc = 1 - c
    in (True, a - b - nc, not $ carry a (-b) nc, overflow a (-b) nc)
  alu RSC a b c _  _ = let nc = 1 - c
    in (True, b - a - nc, not $ carry (-a) b nc, overflow (-a) b nc)
  alu TST a b c sc v = (False, a .&. b, sc, v)
  alu TEQ a b c sc v = (False, a `xor` b, sc, v) 
  alu CMP a b c _  _ = (False, a - b, not $ carry a (-b) 0, overflow a (-b) 0)
  alu CMN a b c _  _ = (False, a + b, carry a b 0, overflow a b 0)
  alu ORR a b c sc v = (True, a .|. b, sc, v)
  alu MOV a b c sc v = (True, b, sc, v)
  alu BIC a b c sc v = (True, a .&. complement b, sc, v)
  alu MVN a b c sc v = (True, complement b, sc, v)

  carry :: Word32 -> Word32 -> Word32 -> Bool
  carry a b c = a64 + b64 + c64 > fromIntegral (maxBound :: Word32)
    where a64 = fromIntegral a :: Word64
          b64 = fromIntegral b :: Word64
          c64 = fromIntegral c :: Word64

  overflow :: Word32 -> Word32 -> Word32 -> Bool
  overflow a b c = fromIntegral (a32 + b32 + c32) == a64 + b64 + c64
    where a32 = fromIntegral a :: Int32
          b32 = fromIntegral b :: Int32
          c32 = fromIntegral c :: Int32
          a64 = fromIntegral a :: Int64
          b64 = fromIntegral b :: Int64
          c64 = fromIntegral c :: Int64

  getFlags :: State Machine (Bool, Bool, Bool, Bool)
  getFlags = gets $ \s -> (c s, n s, v s, z s)

  setFlags :: (Bool, Bool, Bool, Bool) -> State Machine ()
  setFlags (c', n', v', z') = modify $ \s -> s {c = c', n = n', v = v', z = z'}

  getShifterOperand :: ShifterOperand -> State Machine (Word32, Bool)
  getShifterOperand (IM rotate imm) = return
    ((fromIntegral imm) `rotateR` (fromIntegral rotate), False)
  getShifterOperand (SI rm sh amt) = do
    val <- getRegister rm
    let op = case sh of
                  LSL -> shiftL
                  LSR -> shiftR
                  ASR -> \val amt -> fromIntegral $ 
                                      (fromIntegral val :: Int32) `shiftR` amt
                  ROR -> rotateR
    return $ (val `op` (fromIntegral amt), False)
  getShifterOperand so = error $ show so

  getRegister :: Register -> State Machine Word32
  getRegister reg = gets $ \s -> rf s ! reg

  setRegister :: Register -> Word32 -> State Machine ()
  setRegister rd d = modify $ \s -> s {rf = rf s // [(rd, d)]}

  instance Ix Register where
    range (r, s) = [r..s]
    inRange (a, b) r = a <= r && r <= b
    index (a, b) r = fromEnum r - fromEnum a
