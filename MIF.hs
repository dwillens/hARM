module MIF (showMIF, readMIF) where
  import Control.Monad
  import Data.Bits
  import Data.Word
  import Numeric
  import Text.Parsec
  import Text.Parsec.Char
  import Text.Parsec.String
  import Text.Printf
    
  showMIF :: (PrintfArg a, Num a, Bits a) => [a] -> String
  showMIF is =
    concat [mifHeader
           ,unlines $ zipWith (printf mifFormat) addresses is
           ,mifFooter
           ]
    where width :: Int
          width = bitSize $ head is

          depth :: Int
          depth = length is

          mifHeader :: String
          mifHeader = unlines ["DEPTH = " ++ show depth  ++ ";"
                              ,"WIDTH = " ++ show width ++ ";"
                              ,"ADDRESS_RADIX = HEX;"
                              ,"DATA_RADIX = HEX;"
                              ,"CONTENT"
                              ,"BEGIN"
                              ]

          mifFooter :: String
          mifFooter = "END;\n"

          addresses :: [Integer]
          addresses = [0,byteWidth..]
            where byteWidth = (fromIntegral width) `div` 8

          mifFormat :: String
          mifFormat = "%0" ++ show (intLog 16 lastAddress) ++ "x : " ++
                  "%0" ++ show (width `div` 4) ++ "x;"
            where lastAddress = head $ drop (depth - 1) addresses

          intLog :: (Integral a) => a -> a -> a
          intLog b n 
            | n < b = 1
            | otherwise = 1 + intLog b (n `div` b)

  readMIF :: (Read a, Num a, Bits a) => String -> [a]
  readMIF s = case runParser parseMIF (MIF 0 0 read read) "" s of
                   Left e -> error $ show e
                   Right d -> d

  data MIF a = MIF {depth :: Int 
                   ,width :: Int 
                   ,addressRead :: String -> a 
                   ,dataRead :: String -> a 
                   }

  parseMIF :: (Read a, Num a, Eq a) => Parsec String (MIF a) [a]
  parseMIF = do count 4 (do mifProperty; char ';'; spaces)
                string "CONTENT"
                spaces
                string "BEGIN"
                spaces
                d <- liftM depth getState
                ws <- count d parseMIFData
                string "END;"
                spaces
                return ws

  parseMIFData :: (Read a, Num a) => Parsec String (MIF a) a
  parseMIFData = do many hexDigit
                    string " : "
                    dr <- liftM dataRead getState 
                    val <- liftM dr $ many hexDigit
                    char ';'
                    spaces
                    return val


  mifProperty :: (Read a, Num a, Eq a) => Parsec String (MIF a) ()
  mifProperty = do prop <- many1 (letter <|> char '_')
                   string " = "
                   case prop of 
                      "DEPTH" -> do depth <- liftM read $ many1 digit 
                                    modifyState $ \s -> s { depth = depth }
                      "WIDTH" -> do width <- liftM read $ many1 digit 
                                    modifyState $ \s -> s { width = width }
                      "ADDRESS_RADIX" ->
                        do radix <- many1 letter 
                           let readFunc = case radix of
                                               "DEC" -> read 
                                               "OCT" -> fst . head . readOct 
                                               "HEX" -> fst . head . readHex
                           modifyState $ \s -> s { addressRead = readFunc }
                      "DATA_RADIX" ->
                        do radix <- many1 letter
                           let readFunc = case radix of
                                               "DEC" -> read
                                               "OCT" -> fst . head . readOct
                                               "HEX" -> fst . head . readHex
                           modifyState $ \s -> s { dataRead = readFunc }
