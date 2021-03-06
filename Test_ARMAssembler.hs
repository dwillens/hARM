module Main where
  import ARM.Assembler
  import ARM.Disassembler
  import ARM.InstructionSet as I
  import Text.Printf
  import Data.Word
  import Data.List
  import Test.HUnit

  dp =  [([0xE0910002], [DP ADD AL True R0 R1 (SI R2 LSL 0)])
        ,([0x304948AB], [DP SUB CC False R4 R9 (SI R11 LSR 17)])
        ,([0x11973275], [DP ORR NE True R3 R7 (SR R5 ROR R2)])
        ,([0x73778926], [DP CMN VC True R8 R7 (IM 18 38)])
        ]

  mem = [([0xB51F2064], [MEM LDR I.LT False WORD R2 R15 DOWN (MEMI 100)])
        ,([0x87C19FC3], [MEM STR HI False BYTE R9 R1 UP (MEMR R3 ASR 31)])
        ,([0x015CDEFB], [MEM LDR I.EQ True HALF R13 R12 DOWN (MEMI 235)])
        ,([0x011CD0F9], [MEM LDR I.EQ True HALF R13 R12 DOWN (MEMR R9 LSL 0)])
        ,([0xA1C7AADE], [MEM LDR GE False DOUBLE R10 R7 UP (MEMI 174)])
        ,([0xA187A0D2], [MEM LDR GE False DOUBLE R10 R7 UP (MEMR R2 LSL 0)])
        ,([0x41C681F6], [MEM STR MI False DOUBLE R8 R6 UP (MEMI 22)])
        ,([0x418680FB], [MEM STR MI False DOUBLE R8 R6 UP (MEMR R11 LSL 0)])
        ,([0x515C40B7], [MEM LDR PL False HALF R4 R12 DOWN (MEMI 7)])
        ,([0x511C40B8], [MEM LDR PL False HALF R4 R12 DOWN (MEMR R8 LSL 0)])
        ,([0x615A52DA], [MEM LDR VS True BYTE R5 R10 DOWN (MEMI 42)])
        ,([0x611A50DF], [MEM LDR VS True BYTE R5 R10 DOWN (MEMR R15 LSL 0)])
        ,([0xC1CE3EBB], [MEM STR I.GT False HALF R3 R14 UP (MEMI 235)])
        ,([0xC18E30B9], [MEM STR I.GT False HALF R3 R14 UP (MEMR R9 LSL 0)])
        ]

  branch = [([0xEA000000, 0x00000000], 
             [B AL False "0x000000"
             ,I.Label "0x000000" (DP AND I.EQ False R0 R0 (SI R0 LSL 0))])
           ,([0xEAFFFFFC], [I.Label "0xFFFFFC" (B AL False "0xFFFFFC")])
           ,([0x00000000, 0xEAFFFFF8],
             [I.Label "0xFFFFF8" (DP AND I.EQ False R0 R0 (SI R0 LSL 0))
             ,B AL False "0xFFFFF8"])
           ,([0xEB000000, 0x00000000], 
             [B AL True "0x000000" 
             ,I.Label "0x000000" (DP AND I.EQ False R0 R0 (SI R0 LSL 0))])
           ,([0xEBFFFFFC], [I.Label "0xFFFFFC" (B AL True "0xFFFFFC")])
           ,([0x00000000, 0xEBFFFFF8], 
             [I.Label "0xFFFFF8" (DP AND I.EQ False R0 R0 (SI R0 LSL 0)) 
             ,B AL True "0xFFFFF8"])
           ]

  str = [([0x44434241], [DataString "ABCD"])
        ,([0x00004241], [DataString "AB"])
        ,([0x44434241, 0x48474645], [DataString "ABCDEFGH"])
        ,([0xE28FCF01, 0x44434241, 0x48474645], 
          [LoadAddress R12 "world" 
          ,I.Label "hello" (DataString "ABCD")
          ,I.Label "world" (DataString "EFGH")])
        ,([0xE28FCF03, 0x41414141, 0x41414141, 0x41414141, 0x48474645], 
          [LoadAddress R12 "world" 
          ,I.Label "hello" (DataString "AAAAAAAAAAAA")
          ,I.Label "world" (DataString "EFGH")])
        ]

  swi = [([0xEFFFFFFF], [SWI AL (-1)])
        ,([0xEF000001], [SWI AL 1])
        ]

  main = runTestTT $ TestList $ [testAssembler, testDisassembler]


  testAssembler :: Test
  testAssembler = TestList $ map (uncurry test) (dp ++ mem ++ branch ++ str ++ swi)
    where test :: [Word32] -> [Instruction] -> Test
          test ws is = TestCase $ assertEqual (name ws is) ws $ assemble is
          name :: [Word32] -> [Instruction] -> String
          name ws is = "assemble [" ++ commaList (map show is) ++ "] => " ++
                       "[" ++ commaList (map (printf "0x%08x") ws) ++ "]"

  testDisassembler :: Test
  testDisassembler = TestList $ map (uncurry test) (dp ++ mem ++ branch')
    where test :: [Word32] -> [Instruction] -> Test
          test ws is = TestCase $ assertEqual (name ws is) is $ disassemble ws
          name :: [Word32] -> [Instruction] -> String
          name ws is = "disassemble [" ++ commaList (map hex ws) ++ "] => " ++
                       "[" ++ commaList (map show is) ++ "]"
          branch' = map (\(ws, is) -> (ws, map removeLabels is)) branch
          removeLabels :: Instruction -> Instruction
          removeLabels (I.Label _ i) = i
          removeLabels i = i

  commaList :: [String] -> String
  commaList = intercalate ", "

  hex :: Word32 -> String
  hex = printf "0x%08x"
