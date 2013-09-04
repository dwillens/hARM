module ARM.Simulator.Common (Machine(..)
                            ,Bus
                            ,BusDevice(..)
                            ,MemoryDevice(..)
                            ,Action(..)
                            ,reset
                            ,getRegister
                            ,setRegister
                            ,busRead
                            ,busWrite
                            ,makeMemDevice
                            ,step
                            ) where

  import ARM.Disassembler
  import ARM.InstructionSet as I
  import Control.Monad.State
  import Data.Array
  import Data.Array.IO
  import Data.Array.MArray
  import Data.Bits
  import Data.Int
  import Data.Word

  data Machine = Machine {rf :: Array Register Word32
                          ,ir :: Word32
                          ,z :: Bool
                          ,n :: Bool
                          ,c :: Bool
                          ,v :: Bool
                          }
    deriving (Show)


  type Bus = [BusDevice]

  data BusDevice =
    BusDevice {containsAddr :: BusAddress -> Bool
              ,devRead :: BusAddress -> MemSize -> String -> String -> 
                          IO (Word32, BusDevice, String, String)
              ,devWrite :: BusAddress -> MemSize -> Word32 -> 
                            String -> String -> IO (BusDevice, String, String)
              }

  data MemoryDevice = MemoryDevice {memStart :: BusAddress
                                   ,memLen :: BusAddress
                                   ,memory :: IOArray BusAddress Word32
                                   }
  data Action = Stop
              | Continue
              | ReadMem Register BusAddress MemSize Signedness
              | WriteMem Register BusAddress MemSize

  instance Ix Register where
    range (r, s) = [r..s]
    inRange (a, b) r = a <= r && r <= b
    index (a, b) r = fromEnum r - fromEnum a

  reset :: Machine
  reset = Machine 
    {rf = listArray (minBound, maxBound) $ repeat 0 
    ,z = False
    ,n = False
    ,c = False
    ,v = False
    ,ir = 0
    }
  
  getRegister :: Register -> State Machine Word32
  getRegister reg = gets $ \s -> rf s ! reg

  setRegister :: Register -> Word32 -> State Machine ()
  setRegister rd d = modify $ \s -> s {rf = rf s // [(rd, d)]}

  busRead :: Bus -> BusAddress -> MemSize -> String -> String -> 
             IO (Word32, Bus, String, String)
  busRead (dev:devs) addr sz input output
    | not $ aligned sz addr = error $ "Unaligned read " ++ show (addr, sz)
    | dev `containsAddr` addr =
        do (val, dev', input', output') <- devRead dev addr sz input output
           return (val, dev':devs, input', output')
    | otherwise = 
        do (val, devs', input', output') <- busRead devs addr sz input output
           return (val, dev:devs', input', output')

  busWrite :: Bus -> Word32 -> MemSize -> Word32 -> String -> String-> 
              IO (Bus, String, String)
  busWrite (dev:devs) addr sz val input output
    | not $ aligned sz addr = error $ "Unaligned write " ++ show (addr, sz)
    | dev `containsAddr` addr = 
        do (dev', input', output') <- devWrite dev addr sz val input output
           return $ (dev':devs, input', output')
    | otherwise = 
        do (devs', input', output') <- busWrite devs addr sz val input output
           return $ (dev:devs', input', output')

  aligned :: MemSize -> BusAddress -> Bool
  aligned WORD = (0 ==) . (`mod` 4)
  aligned HALF = (0 ==) . (`mod` 2)
  aligned BYTE = const True


  step :: State Machine Action
  step = do   
    i <- liftM disassembleI $ gets ir
    (c, n, v, z) <- getFlags
    --traceShow (i, c, n, v, z) $ return ()
    --rf <- gets rf
    --traceShow rf $ return ()
    if not $ condition i c n v z 
      then return Continue 
      else stepI i

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
       when lnk $ setRegister R14 oldPC
       setRegister R15 $ oldPC + fromIntegral offset
       return Continue

  stepI (MEM op _ sg sz rd rn dir (MEMI offset)) =
    do base <- getRegister rn
       let addr = base + fromIntegral offset
       case op of 
        LDR -> return $ ReadMem rd addr sz sg 
        STR -> return $ WriteMem rd addr sz

  stepI (SWI _ immed24) = 
    if immed24 == 0x00FFFFFF then return Stop else return Continue

  stepI i = error $ show i

  condition :: Instruction -> Bool -> Bool -> Bool -> Bool -> Bool
  condition (DP _ cc _ _ _ _) = condition' cc
  condition (B cc _ _) = condition' cc
  condition (MEM _ cc _ _ _ _ _ _) = condition' cc
  condition (SWI cc _) = condition' cc

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

  makeMemDevice :: MemoryDevice -> BusDevice
  makeMemDevice mem =
    BusDevice {containsAddr = \addr -> memStart mem <= addr &&
                                        addr < memStart mem - memLen mem
              ,devRead =
                \addr sz input output ->
                  do (val, mem') <- memRead mem (addr - memStart mem) sz
                     return (val, makeMemDevice mem', input, output)
              ,devWrite =
                \addr sz val input output ->
                  do mem' <- memWrite mem (addr - memStart mem) sz val
                     return $ (makeMemDevice mem', input, output)
              }

  memRead :: MemoryDevice -> BusAddress -> MemSize -> IO (Word32, MemoryDevice)
  memRead mem addr WORD = do
    val <- readArray (memory mem) $ addr `div` 4
    return (val, mem)

  memRead mem addr BYTE = do
    do val <- readArray (memory mem) (addr `div` 4)
       let shift = fromIntegral $ (addr `mod` 4) * 8
       return ((val `shiftR` shift) .&. 0xFF, mem)

  memWrite :: MemoryDevice -> BusAddress -> MemSize -> Word32 -> IO MemoryDevice
  memWrite mem addr WORD val = do
    writeArray (memory mem) (addr `div` 4) val
    return mem

  memWrite mem addr BYTE val =
    do old <- readArray (memory mem) (addr `div` 4)
       let shift = flip shiftL $ fromIntegral $ (addr `mod` 4) * 8
       let mask = shift 0xFF
       let new = (old .&. complement mask) .|. (shift val .&. mask)
       writeArray (memory mem) (addr `div` 4) new
       return mem

