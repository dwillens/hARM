module ARM.Disassembler (disassemble, disassembleI) where
  import ARM.InstructionSet
  import Data.Bits
  import Data.Word
  import Text.Printf

  disassemble :: [Word32] -> [Instruction]
  disassemble = map disassembleI

  disassembleI :: Word32 -> Instruction
  disassembleI w =
    case intField 25 0x7 of
      0x0 -> 
        if enumField 4 0x1 
          then
            if enumField 7 0x1
              then
                case (intField 20 0x1, intField 6 0x1, intField 5 0x1) of
                  (l, 0, 1)  -> MEM (toEnum l) cc sg HALF rd rn dir mo
                  (0, 1, st) -> MEM (toEnum $ 1 - st) cc sg DOUBLE rd rn dir mo
                  (1, 1, h)  -> MEM LDR cc sg (toEnum $ 1 + h) rd rn dir mo
              else DP dpOp cc dpS rd rn (SR rm sh rs)
          else DP dpOp cc dpS rd rn (SI rm sh shAmt)
      0x1 -> DP dpOp cc dpS rd rn (IM rotate imm8)
      0x2 -> MEM memOp cc False sz rd rn dir (MEMI imm12)
      0x3 -> 
        if enumField 4 0x1
          then undefined
          else MEM memOp cc False sz rd rn dir (MEMR rm sh shAmt)
      0x5 -> B cc lnk bOffset
      otherwise -> undefined
    where enumField :: (Enum a) => Int -> Word32 -> a
          enumField = extractEnum w
          intField :: (Integral a) => Int -> Word32 -> a
          intField = extract w
          cc = enumField 28 0xF
          dpOp = enumField 21 0xF
          memOp = enumField 20 0x1
          dpS = enumField 20 0x1
          rn = enumField 16 0xF
          rd = enumField 12 0xF
          rm = enumField 0 0xF
          shAmt = intField 7 0x1F
          sh = enumField 5 0x3
          rotate = intField 8 0xF `shiftL` 1
          imm8 = intField 0 0xFF
          sz = enumField 22 0x1
          dir = enumField 23 0x1
          imm12 = intField 0 0xFFF
          rs = enumField 8 0xF
          mo = if enumField 22 0x1 then MEMI miscImm else MEMR rm LSL 0
          sg = enumField 20 0x1 && enumField 6 0x1
          miscImm = fromIntegral ((w .&. 0xF00) `shiftR` 4) .|. intField 0 0xF
          lnk = enumField 24 0x1
          bOffset = printf "%06x" $ (intField 0 0xFFFFFF :: Word32)

  extractEnum :: (Enum a) => Word32 -> Int -> Word32 -> a
  extractEnum w s m = toEnum $ extract w s m

  extract :: (Integral a) => Word32 -> Int -> Word32 -> a
  extract w s m = fromIntegral $ (.&. m) $ w `shiftR` s