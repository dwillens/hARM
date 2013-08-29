module ARM.InstructionSet (Instruction(..)
                          ,ConditionCode(..)
                          ,DPOpcode(..)
                          ,Register(..)
                          ,Shift(..)
                          ,ShifterOperand(..)
                          ,MemOpcode(..)
                          ,MemSize(..)
                          ,MemDir(..)
                          ,MemOffset(..)
                          ,Signedness
                          ,BusAddress
                          ) where
  import Data.Word

  type BusAddress = Word32
  
  data Instruction = 
     DP           DPOpcode ConditionCode Bool Register Register ShifterOperand
   | MEM          MemOpcode ConditionCode Signedness MemSize Register Register 
                  MemDir MemOffset
   | B            ConditionCode Bool Label
   | Label        Label Instruction
   | DataString   String
   | LoadAddress  Register Label
    deriving (Eq, Show, Read)

  data ConditionCode = EQ | NE | CS | CC | MI | PL | VS | VC
                     | HI | LS | GE | LT | GT | LE | AL
    deriving (Eq, Show, Read, Enum, Bounded)

  data DPOpcode = AND | EOR | SUB | RSB | ADD | ADC | SBC | RSC
                | TST | TEQ | CMP | CMN | ORR | MOV | BIC | MVN
    deriving (Eq, Show, Read, Enum, Bounded)

  data Register = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7
                | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
    deriving (Eq, Show, Read, Enum, Bounded, Ord)

  data Shift = LSL | LSR | ASR | ROR
    deriving (Eq, Show, Read, Enum, Bounded)

  data ShifterOperand = SI Register Shift Word8
                      | SR Register Shift Register
                      | IM Word8 Word8
    deriving (Eq, Show, Read)

  data MemOpcode = STR | LDR 
    deriving (Eq, Show, Read, Enum, Bounded)

  data MemSize = WORD | BYTE | HALF | DOUBLE
    deriving (Eq, Show, Read, Enum, Bounded)

  data MemOffset = MEMI Word16 | MEMR Register Shift Word8
    deriving (Eq, Show, Read)

  data MemDir = DOWN | UP
    deriving (Eq, Show, Read, Enum)

  type Label = String
  type Signedness = Bool


