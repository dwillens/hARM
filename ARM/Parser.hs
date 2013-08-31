module ARM.Parser (parseARM) where
  import ARM.InstructionSet
  import Control.Monad
  import Data.Bits
  import Data.List
  import Data.Word
  import Text.ParserCombinators.Parsec
  import Text.ParserCombinators.Parsec.Language
  import qualified Text.ParserCombinators.Parsec.Token as Token

  parseARM :: String -> [Instruction]
  parseARM program = case parse (many1 armParser) "" program of
                          Left e -> error $ show e
                          Right is -> is

  armReserved = 
    let ops, ccs, regs, shifts, flags :: [String]
        ccs = map show ([minBound..maxBound] :: [ConditionCode]) ++ ["HS", "LO"]
        regs = map show ([minBound..maxBound] :: [Register])
        shifts = map show ([minBound..maxBound] :: [Shift])
        flags = ["S", "L"]
        ops = map show ([minBound..maxBound] :: [DPOpcode]) ++
              ["B", "LDR", "STR", "SWI", ".la", ".string"]
    in ops ++ ccs ++ regs ++ shifts ++ flags


  armDef = emptyDef {Token.commentStart      = "/*"
                    ,Token.commentEnd        = "*/"
                    ,Token.commentLine       = "//"
                    ,Token.identStart        = letter <|> char '_'
                    ,Token.identLetter       = alphaNum <|> char '_'
                    ,Token.reservedNames     = armReserved
                    }

  lexer = Token.makeTokenParser armDef
  
  whiteSpace    = Token.whiteSpace lexer
  reserved      = Token.reserved lexer
  integer       = Token.integer lexer
  identifier    = Token.identifier lexer
  colon         = Token.colon lexer
  symbol        = Token.symbol lexer
  stringLiteral = Token.stringLiteral lexer

  armParser :: Parser Instruction
  armParser = whiteSpace >> instruction

  instruction :: Parser Instruction
  instruction = 
        dataProcessingInstruction
    <|> branchInstruction 
    <|> memInstruction
    <|> swiInstruction
    <|> loadAddressInstruction
    <|> dataStringInstruction
    <|> (do l <- identifier; colon; i <- instruction; return $ Label l i)

  swiInstruction :: Parser Instruction
  swiInstruction = do reserved "SWI"
                      cc <- option AL conditionCode
                      immed24 <- constant
                      return $ SWI cc immed24

  dataStringInstruction :: Parser Instruction
  dataStringInstruction = do reserved ".string"
                             s <- stringLiteral 
                             return $ DataString s

  loadAddressInstruction :: Parser Instruction
  loadAddressInstruction = do reserved ".la"
                              rd <- register
                              label <- identifier
                              return $ LoadAddress rd label

  branchInstruction :: Parser Instruction
  branchInstruction = do reserved "B"
                         cc <- option AL conditionCode
                         target cc
    where target :: ConditionCode -> Parser Instruction
          target cc = (do link <- option False (reserved "L" >> return True) 
                          target <- identifier 
                          return $ B cc link target)
                  <|> (do rm <- register
                          return $ DP MOV cc False R15 R0 (SI rm LSL 0))

  dataProcessingInstruction :: Parser Instruction
  dataProcessingInstruction = do op <- dpOpcode
                                 let cmp = op `elem` [TST, TEQ, CMP, CMN] 
                                 let mov = op `elem` [MOV, MVN] 
                                 cc <- option AL conditionCode 
                                 s <- option cmp $ reserved "S" >> return True 
                                 rd <- if cmp then return R0 else register 
                                 rn <- if mov then return R0 else register 
                                 so <- shifterOperand 
                                 return $ DP op cc s rd rn so
    where dpOpcode :: Parser DPOpcode
          dpOpcode = parseEnum

  memInstruction :: Parser Instruction
  memInstruction = do op <- memOpcode
                      cc <- option AL conditionCode
                      sg <- option False $ reserved "S" >> return True
                      sz <- option WORD memSize
                      rd <- register
                      rn <- register
                      (dir, offset) <- memOffset
                      return $ MEM op cc sg sz rd rn dir offset
    where memOpcode :: Parser MemOpcode
          memOpcode = parseEnum
          memSize :: Parser MemSize
          memSize = parseEnum
          memOffset :: Parser (MemDir, MemOffset)
          memOffset = (do n <- constant :: Parser Integer
                          return (if n >= 0 then UP else DOWN,
                                  MEMI $ fromInteger $ abs n))
                  <|> (do dir <- option UP ((symbol "-" >> return DOWN) <|>
                                            (symbol "+" >> return UP))
                          so <- shifterOperand
                          case so of
                            SI rm sh amt   -> return (dir, MEMR rm sh amt)
                            _              -> error $ "Invalid shift for " ++
                                                      "memory op " ++ show so)
                  <|> return (UP, MEMI 0)


  shifterOperand :: Parser ShifterOperand
  shifterOperand = (do rm <- register
                       option (SI rm LSL 0) $ do
                       sh <- shiftOp 
                       liftM (SR rm sh) register <|> liftM (SI rm sh) constant)
               <|> liftM immediate constant
    where immediate :: Word32 -> ShifterOperand
          immediate n = IM (fromIntegral amt) $ fromIntegral (n `rotateL` amt)
            where shifts = zipWith rotateL (repeat n) [0,2..30]
                  amt = 2 * ix
                  ix = case findIndex (\x -> x == x .&. 0xFF) shifts of
                            Just ix -> ix
                            Nothing -> error $ "Couldn't form immediate " ++
                                               "from " ++ show n
  
  constant :: (Integral a) => Parser a
  constant = do symbol "#"; liftM fromIntegral integer

  register :: Parser Register
  register = parseEnum

  shiftOp :: Parser Shift
  shiftOp = parseEnum

  conditionCode :: Parser ConditionCode
  conditionCode = parseEnum 
              <|> (reserved "HS" >> return CS)
              <|> (reserved "LO" >> return CC)

  parseEnum :: (Enum e, Show e, Bounded e) => Parser e
  parseEnum = foldl1 (<|>) . map (\r -> reserved (show r) >> return r) $
                                 [minBound..maxBound]
