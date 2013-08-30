module Main where
  import ARM.InstructionSet as ARM
  import ARM.Parser
  import Test.HUnit

  instructions :: [([Instruction], String)]
  instructions =
    [([DP ADD AL False R0 R1 (SI R2 LSL 0)], "ADD AL R0 R1 R2")
    ,([DP SUB NE False R0 R1 (SI R2 LSL 0)], "SUB NE R0 R1 R2")
    ,([DP ORR ARM.EQ False R0 R1 (SI R2 LSL 0)], "ORR EQ R0 R1 R2")
    ,([DP AND AL False R0 R1 (SI R2 LSL 0)], "AND R0 R1 R2")
    ,([DP EOR AL True R0 R1 (SI R2 LSL 0)], "EOR S R0 R1 R2")
    ,([DP RSB AL True R5 R1 (SI R2 LSL 0)], "RSB S R5 R1 R2")
    ,([DP ADC AL False R5 R4 (SI R2 LSL 0)], "ADC R5 R4 R2")
    ,([DP SBC AL False R5 R4 (SI R3 LSL 0)], "SBC R5 R4 R3")
    ,([DP RSC AL False R5 R4 (SI R3 LSL 23)], "RSC R5 R4 R3 LSL #23")
    ,([DP TST AL True R0 R5 (SI R3 LSR 10)], "TST R5 R3 LSR #10")
    ,([DP TEQ AL True R0 R5 (SR R3 ASR R9)], "TEQ S R5 R3 ASR R9")
    ,([DP CMP AL True R0 R5 (SR R3 ROR R8)], "CMP R5 R3 ROR R8")
    ,([DP CMN AL True R0 R5 (IM 0 0xFF)], "CMN R5 #0xFF")
    ,([DP ORR AL True R5 R6 (IM 4 0xFF)], "ORR S R5 R6 #0xF000000F")
    ,([DP MOV AL False R5 R0 (IM 2 0xFF)], "MOV R5 #0xC000003F")
    ,([DP BIC AL False R5 R0 (IM 2 0x5A)], "BIC R5 R0 #0x80000016")
    ,([DP MVN AL False R5 R0 (SI R15 ROR 31)], "MVN R5 R15 ROR #31")

    ,([ARM.Label "main" $ B AL False "main"], "main: B main")
    ,([ARM.Label "main2" $ ARM.Label "main" $ B AL False "main"]
     ,"main2: main: B main")
    ,([ARM.Label "main" $ DP ADD AL False R0 R1 (SI R2 LSL 0)
      ,B AL False "main"]
     ,"main: ADD R0 R1 R2\nB main")
    ,([DP MOV AL False R15 R0 (SI R14 LSL 0)], "B R14")

    ,([MEM LDR NE True BYTE R9 R6 UP (MEMI 95)], 
      "LDR NE S BYTE R9 R6 #+95")
    ,([MEM LDR AL False WORD R2 R15 DOWN (MEMI 100)], 
      "LDR R2 R15 #-100")
    ,([MEM LDR AL True HALF R4 R1 UP (MEMI 0)], 
      "LDR S HALF R4 R1")
    ,([MEM STR ARM.GT True WORD R8 R2 UP (MEMR R4 LSL 0)], 
      "STR GT S WORD R8 R2 R4")
    ,([MEM STR MI True BYTE R8 R2 UP (MEMR R4 ROR 17)], 
      "STR MI S BYTE R8 R2 R4 ROR #17")

    ,([DP ADD AL False R0 R1 (SI R2 LSL 0)], "ADD AL R0 R1 R2 // comment")
    ,([DP ADD AL False R0 R1 (SI R2 LSL 0)], "ADD AL /* always */ R0 R1 R2")

    ,([LoadAddress R5 "blah"], ".la R5 blah")

    ,([SWI AL (-1)], "SWI #-1")
    ,([SWI ARM.EQ 1], "SWI EQ #1")
    ]

  main = runTestTT $ TestList $ map test instructions
    where test :: ([Instruction], String) -> Test
          test (is, s) = TestCase $ assertEqual s is $ parseARM s