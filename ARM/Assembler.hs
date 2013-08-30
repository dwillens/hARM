module ARM.Assembler (assemble) where
  import ARM.InstructionSet
  import Data.Bits
  import Data.List
  import qualified Data.Map as M
  import Data.Word
  
  type Address = Integer
  type Labels = M.Map String Address

  assemble :: [Instruction] -> [Word32]
  assemble is = concat $ map (uncurry $ assembleI ls) ais
    where ais = address 0 is
          ls = foldr (uncurry insertLabel) M.empty ais
          insertLabel :: Address -> Instruction -> Labels -> Labels
          insertLabel a (Label l i) ls = insertLabel a i $ M.insert l a ls
          insertLabel _ _ ls = ls
          address :: Address -> [Instruction] -> [(Address, Instruction)]
          address a (i:is) = (a,i) : address (a + iSize i) is
          address _ _ = []
          iSize :: (Integral a) => Instruction -> a
          iSize (DataString s) = (genericLength s + 3) `div` 4 * 4
          iSize (Label _ i) = iSize i
          iSize _ = 4

  assembleI :: M.Map String Address -> Address -> Instruction -> [Word32]
  assembleI _ _ (DP op cc s rd rn so) = 
        [dp op cc s rn rd  .|. shifterOperand so]
    where dp :: DPOpcode -> ConditionCode -> Bool -> Register -> Register -> 
                Word32 
          dp op cc s rn rd = base cc rn rd       .|. 
                             enumField 21 0xF op .|. 
                             enumField 20 0x1 s


  assembleI _ _ (MEM op cc sg sz rd rn dir o)
    | isMemBase op sz sg    = [mem cc rn rd dir .|. memBase op sz o]
    | shiftless o           = [mem cc rn rd dir .|. memMisc op sz sg o]
    | otherwise             = error "Cannot shift in miscellaneous load/store"
    where shiftless :: MemOffset -> Bool
          shiftless (MEMI _)         = True
          shiftless (MEMR _ LSL 0)   = True
          shiftless _                = False 
          mem :: ConditionCode -> Register -> Register -> MemDir -> Word32
          mem cc rn rd dir = base cc rn rd        .|.
                             0x01000000           .|.
                             enumField 23 0x1 dir
          isMemBase :: MemOpcode -> MemSize -> Signedness -> Bool
          isMemBase _    WORD  _     = True
          isMemBase _    BYTE  False = True
          isMemBase STR  BYTE  _     = True
          isMemBase _    _     _     = False
          memBase :: MemOpcode -> MemSize -> MemOffset -> Word32 
          memBase op sz o = 0x04000000          .|.
                            enumField 22 0x1 sz .|. 
                            enumField 20 0x1 op .|.
                            offset o
            where offset (MEMI o)         = intField 0 0xFFF o
                  offset (MEMR rm sh amt) = 0x02000000                    .|.
                                            shifterOperand (SI rm sh amt)
          memMisc :: MemOpcode -> MemSize -> Signedness -> MemOffset -> Word32
          memMisc op sz sg o = 0x00000090        .|. 
                               memMisc' op sz sg .|.
                               offset o
            where memMisc' STR HALF     _     = 0x00000020
                  memMisc' LDR DOUBLE   _     = 0x00000040 
                  memMisc' STR DOUBLE   _     = 0x00000060 
                  memMisc' LDR HALF     False = 0x00100020 
                  memMisc' LDR BYTE     True  = 0x00100040 
                  memMisc' LDR HALF     True  = 0x00100060
                  offset (MEMR rm _ _)        = enumField 0 0xF rm 
                  offset (MEMI o)             = 0x00400000        .|. 
                                                intField 4 0xF0 o .|. 
                                                intField 0 0xF  o

  assembleI ls addr (B cc lnk l)
    | M.member l ls   = [0x0A000000             .|. 
                        enumField 28 0xF cc     .|. 
                        enumField 24 0x1 lnk    .|. 
                        offset (M.lookup l ls)]
    | otherwise       = error $ "Label not found: " ++ l
    where offset (Just t) = intField 0 0xFFFFFF $ t - addr - 4

  assembleI _ _ (SWI cc immed24) =
    [enumField 28 0xF cc .|. 0x0F000000 .|. (immed24 .&. 0x00FFFFFF)]

  assembleI ls addr (Label l i) = assembleI ls addr i

  assembleI ls addr (DataString []) = []
  assembleI ls addr (DataString s) = w : ws
    where cs = map fromIntegral . map fromEnum $ take 4 s
          w = foldr (.|.) 0 $ zipWith shiftL cs [0,8..]
          ws = assembleI ls addr $ DataString $ drop 4 s
  
  assembleI ls addr (LoadAddress rd l)
    | M.member l ls   = assembleI ls addr $ 
                  DP op AL False rd R15 $ IM 30 offset
    | otherwise       = error $ "Label not found: " ++ l
    where (Just t) = M.lookup l ls
          o = fromIntegral $ (t - addr - 4)
          op = if o > 0 then ADD else SUB
          offset = if -1020 <= o && o <= 1020 && o `mod` 4 == 0
                    then (abs o) `shiftR` 2
                    else error $ "Target is too far away: " ++ l



  shifterOperand :: ShifterOperand => Word32
  shifterOperand (SI rm sh amt)    = intField 7 0x1F amt   .|. 
                                     enumField 5 0x3 sh    .|. 
                                     enumField 0 0xF rm
  shifterOperand (SR rm sh rs)     = 0x00000010            .|.
                                     enumField 8 0xF rs    .|.
                                     enumField 5 0x3 sh    .|.
                                     enumField 0 0xF rm
  shifterOperand (IM amt imm)      = 0x02000000            .|.
                                     intField 7 0x1E amt   .|. 
                                     intField 0 0xFF imm

  base :: ConditionCode -> Register -> Register -> Word32
  base cc rn rd = enumField 28 0xF cc  .|. 
                  enumField 16 0xF rn  .|. 
                  enumField 12 0xF rd

  enumField :: (Enum a) => Int -> Word32 -> a -> Word32
  enumField s m = intField s m . fromEnum

  intField :: (Integral a) => Int -> Word32 -> a -> Word32
  intField s m = (`shiftL` s) . (.&. m) . fromIntegral
