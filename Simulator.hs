module Main where
  import ARM.Disassembler
  import ARM.InstructionSet
  import Control.Monad.State
  import Data.Array
  import Data.Bits
  import Data.Int
  import Data.Word
  import Debug.Trace

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
  memSize = 512

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
  
  main = do
    let program = [0xE280000A, 0xE2400001, 0xEAFFFFF8]
    interact $ flip evalState atReset . execute program

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
    case i of
      DP op cc s rd rn (IM rotate imm) -> 
        do op1 <- getRegister rn
           let op2 = (fromIntegral imm) `rotateR` (fromIntegral rotate)
           let opr = case op of
                          AND -> (.&.)
                          EOR -> xor
                          SUB -> (-)
                          RSB -> subtract
                          ADD -> (+)
                          ADC -> (+)
                          SBC -> (-)
                          RSC -> subtract
                          TST -> (.&.)
                          TEQ -> xor 
                          CMP -> (-)
                          CMN -> (+)
                          ORR -> (.|.)
                          MOV -> flip const
                          BIC -> \a b -> a .&. complement b
                          MVN -> \_ b -> complement b
           setRegister R0 (op1 `opr` op2)
      B cc lnk offsetStr ->
        do let offset = (read offsetStr :: Int32) `shiftL` 8 `shiftR` 8
           let newPC = pc + fromIntegral offset
           setRegister R15 $ newPC
      otherwise -> error $ show i
    return ()

  getMem :: (Memory m) => (Machine -> m) -> Word32 -> State Machine Word32
  getMem m a = state (\s -> (readMem (m s) a, s))

  getMemory = getMem memory
  getRegister :: Register -> State Machine Word32
  getRegister = getMem rf . fromIntegral . fromEnum

  setMemory :: Word32 -> Word32 -> State Machine ()
  setMemory a d = state (\s -> ((), s {memory = writeMem (memory s) a d}))

  setRegister :: Register -> Word32 -> State Machine ()
  setRegister r d = state (\s -> 
    ((), s {rf = writeMem (rf s) ((fromIntegral . fromEnum) r) d}))
