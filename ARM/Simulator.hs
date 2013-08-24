module ARM.Simulator (simulate) where
  import ARM.Disassembler
  import ARM.InstructionSet as I
  import Control.Monad.State
  import Data.Array
  import Data.Bits
  import Data.Int
  import Data.Word

  import Debug.Trace
  import Text.Printf

  data Machine = Machine {memory :: ArrayMemory
                         ,rf :: ArrayMemory
                         ,z :: Bool
                         ,n :: Bool
                         ,c :: Bool
                         ,v :: Bool
                         ,input :: String
                         ,output :: String
                         }
    deriving (Show)

  newtype ArrayMemory = ArrayMemory (Array Word32 Word32)
    deriving (Show)

  class Memory m where
    readMem :: m -> Word32 -> Word32
    writeMem :: m -> Word32 -> Word32 -> m

  instance Memory ArrayMemory where
    readMem (ArrayMemory m) = (m !)
    writeMem (ArrayMemory m) a d = ArrayMemory $ m // [(a, d)]

  memSize :: (Integral a) => a
  memSize = 0x80000

  rfSize :: (Integral a) => a
  rfSize = 16

  initialize :: [Word32] -> State Machine ()
  initialize ws = state $ \s -> ((), 
    s {memory = foldl write (memory s) (zip [0,4..] ws)})
    where write :: (Memory m) => m -> (Word32, Word32) -> m
          write m (a, d) = writeMem m a d

  atReset :: Machine
  atReset = Machine 
    {memory = ArrayMemory $ listArray (0, memSize - 1) $ replicate memSize 0 
    ,rf = ArrayMemory $ listArray (0, rfSize - 1) $ replicate rfSize 0 
    ,z = False
    ,n = False
    ,c = False
    ,v = False
    ,input = ""
    ,output = ""
    }
  
  reset :: String -> State Machine ()
  reset input = state $ \_ -> ((), atReset { input = input })
  
  simulate :: [Word32] -> String -> String
  simulate program = flip evalState atReset . execute program

  execute :: [Word32] -> String -> State Machine String
  execute program input = do
    reset input
    initialize program
    runUntil (\m -> readMem (rf m) 15 == 0)
    state $ (\s -> (output s, s))

  runUntil :: (Machine -> Bool) -> State Machine ()
  runUntil p = do
      step
      done <- isDone p
      if done then return () else runUntil p
    where isDone :: (Machine -> Bool) -> State Machine Bool
          isDone p = state $ \s -> (p s, s)

  step :: State Machine ()
  step = do
    pc <- getRegister R15
    ir <- getMemory pc
    setRegister R15 (pc + 4)
    let i = disassembleI ir
    c <- getCarry
    n <- getNegative
    v <- getOverflow
    z <- getZero
    let ci = fromIntegral $ fromEnum c
    --r0 <- getRegister R0
    --r8 <- getRegister R8
    --sp <- getRegister R13
    --traceShow (pc, i, c, n, v, z, sp, r0, r8) $ return ()
    if shouldExec i c n v z then
      case i of
        DP op cc s rd rn so ->
          do op1 <- getRegister rn
             (op2, sc) <- getShifterOperand so
             let (wb, result, c', v') = decodeDP op op1 op2 ci sc v
             let n' = isNegative result
             let z' = result == 0
             if s 
              then do setCarry c'
                      setNegative n'
                      setOverflow v'
                      setZero z'
              else return ()
             if wb then setRegister rd result else return ()
        B cc lnk offsetStr ->
          do let offset = (read offsetStr :: Int32) `shiftL` 8 `shiftR` 8
             oldPC <- getRegister R15
             if lnk then setRegister R14 oldPC else return () 
             let newPC = oldPC + fromIntegral offset
             setRegister R15 $ newPC
        MEM LDR cc sg sz rd rn dir (MEMI offset) -> 
          do base <- getRegister rn
             let addr = base + (fromIntegral offset)
             let wordAddr = addr .&. complement 0x3
             let subWordAddr = addr .&. 0x3
             let shift = fromIntegral (subWordAddr * 8)
             case sz of
              WORD -> 
                do val <- getMemory wordAddr
                   setRegister rd val
              BYTE ->
                do val <- case wordAddr of
                            0x00FF00 -> case subWordAddr of
                                          0x0 -> return 0x0F
                                          0x2 -> liftM (flip shiftL 16) getInputByte
                                          otherwise -> return 0x00
                            0x00FF04 -> case subWordAddr of
                                          0x0 -> getInputByte 
                                          otherwise -> return 0x00
                            otherwise -> getMemory wordAddr
                   --traceShow ("LDR", addr, wordAddr, subWordAddr, val) $ return () 
                   setRegister rd $ (val `shiftR` shift) .&. 0xFF
        MEM STR cc sg sz rd rn dir (MEMI offset) -> 
          do val <- getRegister rd
             base <- getRegister rn
             let addr = base + (fromIntegral offset)
             let wordAddr = addr .&. complement 0x3
             let subWordAddr = addr .&. 0x3
             let shift = fromIntegral (subWordAddr * 8)
             case sz of
              WORD -> setMemory wordAddr val
              BYTE -> do --traceShow ("STR", addr, wordAddr, subWordAddr, val) $ return ()
                         oldVal <- getMemory wordAddr
                         let old = oldVal .&. complement (0xFF `shiftL` shift)
                         case wordAddr of
                            0x00FF00 -> case subWordAddr of 
                                              0x0 -> if val .&. 0xA /= 0 
                                                      then do w <- getMemory 0x00FF00
                                                              putOutputByte w
                                                      else return ()
                                              0x2 -> setMemory 0x00FF00 val 
                                              otherwise -> return ()
                            0x00FF04 -> case subWordAddr of
                                              0x0 -> setMemory 0x00FF00 val
                                              otherwise -> return ()
                            otherwise -> setMemory wordAddr $ old .|. (val `shiftL` shift)
        otherwise -> error $ show i
      else return ()
    return ()

  shouldExec :: Instruction -> Bool -> Bool -> Bool -> Bool -> Bool
  shouldExec (DP _ cc _ _ _ _) = shouldExec' cc
  shouldExec (B cc _ _) = shouldExec' cc
  shouldExec (MEM _ cc _ _ _ _ _ _) = shouldExec' cc

  shouldExec' :: ConditionCode -> Bool -> Bool -> Bool -> Bool -> Bool
  shouldExec' I.EQ _ _ _ z = z
  shouldExec' I.NE _ _ _ z = not z
  shouldExec'   CS c _ _ _ = c
  shouldExec'   CC c _ _ _ = not c
  shouldExec'   MI _ n _ _ = n
  shouldExec'   PL _ n _ _ = not n
  shouldExec'   VS _ _ v _ = v
  shouldExec'   VC _ _ v _ = not v
  shouldExec'   HI c _ _ z = c && not z
  shouldExec'   LS c _ _ z = not c || z
  shouldExec'   GE _ n v _ = n == v
  shouldExec' I.LT _ n v _ = n /= v
  shouldExec' I.GT _ n v z = n == v && not z
  shouldExec'   LE _ n v z = n /= v || z
  shouldExec'   AL _ _ _ _ = True

  decodeDP :: DPOpcode -> Word32 -> Word32 -> Word32 -> Bool -> Bool -> 
              (Bool, Word32, Bool, Bool)
  decodeDP AND a b c sc v = (True, a .&. b, sc, v)
  decodeDP EOR a b c sc v = (True, a `xor` b, sc, v)
  decodeDP SUB a b c sc v = (True, a - b, not $ carry a (-b) 0, overflow a (-b) 0)
  decodeDP RSB a b c sc v = (True, b - a, not $ carry (-a) b 0, overflow (-a) b 0)
  decodeDP ADD a b c sc v = (True, a + b, carry a b 0, overflow a b 0)
  decodeDP ADC a b c sc v = (True, a + b + c, carry a b c, overflow a b c)
  decodeDP SBC a b c sc v = (True, a - b - nc, not $ carry a (-b) nc, overflow a (-b) nc)
    where nc = 1 - c
  decodeDP RSC a b c sc v = (True, b - a - nc, not $ carry (-a) b nc, overflow (-a) b nc)
    where nc = 1 - c
  decodeDP TST a b c sc v = (False, a .&. b, sc, v)
  decodeDP TEQ a b c sc v = (False, a `xor` b, sc, v) 
  decodeDP CMP a b c sc v = (False, a - b, not $ carry a (-b) 0, overflow a (-b) 0)
  decodeDP CMN a b c sc v = (False, a + b, carry a b 0, overflow a b 0)
  decodeDP ORR a b c sc v = (True, a .|. b, sc, v)
  decodeDP MOV a b c sc v = (True, b, sc, v)
  decodeDP BIC a b c sc v = (True, a .&. complement b, sc, v)
  decodeDP MVN a b c sc v = (True, complement b, sc, v)

  carry :: Word32 -> Word32 -> Word32 -> Bool
  carry a b c = a64 + b64 + c64 > fromIntegral (maxBound :: Word32)
    where a64 = fromIntegral a :: Word64
          b64 = fromIntegral b :: Word64
          c64 = fromIntegral c :: Word64

  overflow :: Word32 -> Word32 -> Word32 -> Bool
  overflow a b c = False

  isNegative :: Word32 -> Bool
  isNegative = (> maxBound `div` 2)

  getFlag :: (Machine -> Bool) -> State Machine Bool
  getFlag f = state $ \s -> (f s, s)

  getCarry, getOverflow, getNegative, getZero :: State Machine Bool
  getCarry = getFlag c
  getOverflow = getFlag v
  getNegative = getFlag n
  getZero = getFlag z

  setCarry, setOverflow, setNegative, setZero :: Bool -> State Machine ()
  setCarry b = state $ \s -> ((), s { c = b })
  setOverflow b = state $ \s -> ((), s { v = b })
  setNegative b = state $ \s -> ((), s { n = b })
  setZero b = state $ \s -> ((), s { z = b })

  getInputByte :: State Machine Word32
  getInputByte = state $ \s -> (fromIntegral $ fromEnum $ head $ input s, 
                                s { input = tail $ input s})

  putOutputByte :: Word32 -> State Machine ()
  putOutputByte val = state $ \s -> 
    ((), s { output = (output s ++ [toEnum $ fromIntegral val])})

  getShifterOperand :: ShifterOperand -> State Machine (Word32, Bool)
  getShifterOperand (IM rotate imm) = state $ \s ->
    (((fromIntegral imm) `rotateR` (fromIntegral rotate), False), s)
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

  getMem :: (Memory m) => (Machine -> m) -> Word32 -> State Machine Word32
  getMem m a = state (\s -> (readMem (m s) a, s))

  getMemory :: Word32 -> State Machine Word32
  getMemory = getMem memory

  getRegister :: Register -> State Machine Word32
  getRegister = getMem rf . fromIntegral . fromEnum

  setMemory :: Word32 -> Word32 -> State Machine ()
  setMemory a d = state (\s -> ((), s {memory = writeMem (memory s) a d}))

  setRegister :: Register -> Word32 -> State Machine ()
  setRegister r d = state (\s -> 
    ((), s {rf = writeMem (rf s) ((fromIntegral . fromEnum) r) d}))
