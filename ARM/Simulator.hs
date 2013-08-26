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
                         ,input :: Maybe Char
                         ,output :: Maybe Char
                         }
    deriving (Show)

  newtype ArrayMemory = ArrayMemory {mem :: Array Word32 Word32}
    deriving (Show)

  class Memory m where
    readMem :: Word32 -> m -> Word32
    writeMem :: Word32 -> Word32 -> m -> m

  instance Memory ArrayMemory where
    readMem d = (! d) . mem
    writeMem a d = ArrayMemory . flip (//) [(a, d)] . mem

  memSize :: (Integral a) => a
  memSize = 0x80000

  rfSize :: (Integral a) => a
  rfSize = 16

  initialize :: [Word32] -> Machine -> Machine
  initialize program s =
    s {memory = foldr (uncurry writeMem) (memory s) $ zip [0,4..] program}

  reset :: Machine
  reset = Machine 
    {memory = ArrayMemory $ listArray (0, memSize - 1) $ replicate memSize 0 
    ,rf = ArrayMemory $ listArray (0, rfSize - 1) $ replicate rfSize 0 
    ,z = False
    ,n = False
    ,c = False
    ,v = False
    ,input = Nothing
    ,output = Nothing
    }
  
  simulate :: [Word32] -> IO ()
  simulate program = simulate' $ initialize program reset
    where simulate' :: Machine -> IO ()
          simulate' machine = do
            let (action, machine') = runState execute machine
            case action of
              Nothing -> return ()
              Just (Left input) -> do c <- getChar
                                      --traceShow c $ return ()
                                      simulate' $ input c machine'
              Just (Right output) -> do 
                                        --traceShow output $ return ()
                                        putChar output
                                        simulate' machine'

  execute :: State Machine Action
  execute = runUntil $ (== 0) . readMem 15 . rf

  type Action = Maybe (Either (Char -> Machine -> Machine) Char)

  runUntil :: (Machine -> Bool) -> State Machine Action
  runUntil p = do
      action <- step
      stop <- gets p
      if stop 
        then return Nothing
        else case action of
                  Nothing -> runUntil p
                  otherwise -> return action

  
  step :: State Machine Action
  step = do
    (pc, ir) <- fetchInstruction
    let i = disassembleI ir
    (c, n, v, z) <- getFlags
    --r0 <- getRegister R0
    --r8 <- getRegister R8
    --sp <- getRegister R13
    --traceShow (pc, i, c, n, v, z, sp, r0, r8) $ return ()
    if not $ condition i c n v z then return Nothing else
      case i of
        DP op _ s rd rn so ->
          do op1 <- getRegister rn
             (op2, sc) <- getShifterOperand so
             let ci = fromIntegral $ fromEnum c
             let (wb, result, c', v') = decodeDP op op1 op2 ci sc v
             let n' = result > maxBound `div` 2
             let z' = result == 0
             if s then setFlags (c', n', v', z') else return ()
             if wb then setRegister rd result else return ()
             return Nothing
        B _ lnk offsetStr ->
          do let offset = (read offsetStr :: Int32) `shiftL` 8 `shiftR` 8
             oldPC <- getRegister R15
             if lnk then setRegister R14 oldPC else return () 
             setRegister R15 $ oldPC + fromIntegral offset
             return Nothing
        MEM op _ sg sz rd rn dir (MEMI offset) -> 
          do base <- getRegister rn
             let addr = base + fromIntegral offset
             let wordAddr = addr .&. complement 0x3
             let subWordAddr = addr .&. 0x3
             let shift = fromIntegral $ subWordAddr * 8
             execMem op sg sz rd wordAddr shift
        otherwise -> error $ show i

  execMem :: MemOpcode -> Signedness -> MemSize -> Register ->
               Word32 ->  Int -> State Machine Action
  execMem LDR _ WORD rd wordAddr _ =
    do val <- getMemory wordAddr; setRegister rd val; return Nothing
  execMem STR _ WORD rd wordAddr _ =
    do val <- getRegister rd; setMemory wordAddr val; return Nothing

  execMem LDR False BYTE rd wordAddr shift =
    do curInput <- gets input
       case (wordAddr, shift, curInput) of
            (0x00FF00, 16, Nothing) -> 
              do pc <- getRegister R15 
                 setRegister R15 (pc - 4) 
                 return $ Just $ Left (\c s -> s { input = Just c })
            otherwise -> 
              do val <- case wordAddr of 
                             0x00FF00 -> case shift of 
                                              0  -> return 0x0F
                                              16 -> liftM (flip shiftL 16 . fromIntegral) getInputByte
                                              otherwise -> return 0x00
                             0x00FF04 -> case shift of
                                              0 -> liftM fromIntegral getInputByte 
                                              otherwise -> return 0x00
                             otherwise -> getMemory wordAddr
                 --traceShow ("LDR", wordAddr, shift, val) $ return () 
                 setRegister rd $ (val `shiftR` shift) .&. 0xFF
                 return Nothing

  execMem STR False BYTE rd wordAddr shift = 
    do val <- getRegister rd
       --traceShow ("STR", wordAddr, shift, val) $ return () 
       oldVal <- getMemory wordAddr 
       let old = oldVal .&. complement (0xFF `shiftL` shift) 
       case wordAddr of 
        0x00FF00 -> case shift of 
                      0  -> if val .&. 0xA /= 0 
                              then do c <- gets output
                                      modify $ \s -> s { output = Nothing }
                                      case c of
                                        Nothing -> error "No output"
                                        Just c -> return $ Just $ Right $ c
                              else if val .&. 0x5 /= 0 
                                    then do modify $ \s -> s { input = Nothing }
                                            return Nothing
                                    else return Nothing
                      16 -> do putOutputByte $ fromIntegral val
                               return Nothing
                      otherwise -> return Nothing
        0x00FF04 -> case shift of 
                      0 -> do putOutputByte $ fromIntegral val
                              return Nothing
                      otherwise -> return Nothing
        otherwise -> do setMemory wordAddr $ old .|. (val `shiftL` shift)
                        return Nothing

  fetchInstruction :: State Machine (Word32, Word32)
  fetchInstruction = do pc <- getRegister R15
                        ir <- getMemory pc
                        setRegister R15 (pc + 4)
                        return (pc, ir)

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

  decodeDP :: DPOpcode -> Word32 -> Word32 -> Word32 -> Bool -> Bool -> 
              (Bool, Word32, Bool, Bool)
  decodeDP AND a b c sc v = (True, a .&. b, sc, v)
  decodeDP EOR a b c sc v = (True, a `xor` b, sc, v)
  decodeDP SUB a b c sc v = (True, a - b, 
                             not $ carry a (-b) 0, overflow a (-b) 0)
  decodeDP RSB a b c sc v = (True, b - a, 
                             not $ carry (-a) b 0, overflow (-a) b 0)
  decodeDP ADD a b c sc v = (True, a + b, carry a b 0, overflow a b 0)
  decodeDP ADC a b c sc v = (True, a + b + c, carry a b c, overflow a b c)
  decodeDP SBC a b c sc v = (True, a - b - nc, 
                             not $ carry a (-b) nc, overflow a (-b) nc)
    where nc = 1 - c
  decodeDP RSC a b c sc v = (True, b - a - nc, 
                             not $ carry (-a) b nc, overflow (-a) b nc)
    where nc = 1 - c
  decodeDP TST a b c sc v = (False, a .&. b, sc, v)
  decodeDP TEQ a b c sc v = (False, a `xor` b, sc, v) 
  decodeDP CMP a b c sc v = (False, a - b, 
                             not $ carry a (-b) 0, overflow a (-b) 0)
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

  getInputByte :: State Machine Word8
  getInputByte = gets $ fromIntegral . fromEnum . check . input
    where check :: Maybe Char -> Char
          check (Just c) = c
          check _ = error "No input"

  putOutputByte :: Word8 -> State Machine ()
  putOutputByte val = 
    modify $ \s -> s { output = Just $ toEnum $ fromIntegral val }

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

  getMem :: (Memory m) => (Machine -> m) -> Word32 -> State Machine Word32
  getMem m a = gets $ readMem a . m

  getMemory :: Word32 -> State Machine Word32
  getMemory = getMem memory

  getRegister :: Register -> State Machine Word32
  getRegister = getMem rf . fromIntegral . fromEnum

  setMemory :: Word32 -> Word32 -> State Machine ()
  setMemory a d = modify $ \s -> s {memory = writeMem a d $ memory s}

  setRegister :: Register -> Word32 -> State Machine ()
  setRegister rd d = modify $ \s -> s {rf = writeMem a d $ rf s}
    where a = fromIntegral $ fromEnum rd
