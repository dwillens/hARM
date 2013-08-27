module Main where
  import ARM.Disassembler
  import Data.List
  import MIF
  import System.Environment
  import System.IO
  import Text.Printf

  main = do
    (input:_) <- getArgs
    let (Just baseNameRev) = stripPrefix "fim." . reverse $ input
        output = reverse ("sid." ++ baseNameRev)
    contents <- readFile input
    let buffer = readMIF contents
    let instructions = disassemble buffer
    let line = printf "%06x %s" :: Int -> String -> String
    writeFile output $ unlines $ zipWith line [0,4..] $ map show instructions
