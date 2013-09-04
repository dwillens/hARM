module Main where

  import ARM.Simulator.Gloss
  import ARM.Assembler
  import ARM.Parser
  import Data.List
  import MIF
  import System.Environment
  import System.IO

  main = do
    (input:_) <- getArgs
    contents <- readFile input
    let program = if ".s" `isSuffixOf` input 
                    then assemble $ parseARM contents
                    else readMIF contents
    simulate program
