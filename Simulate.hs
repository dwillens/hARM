module Main where
  import ARM.Simulator
  import ARM.Assembler
  import ARM.Parser
  import Data.List
  import System.Environment
  import System.IO

  main = do
    (input:_) <- getArgs
    contents <- readFile input
    let instructions = parseARM contents
        program = assemble instructions
    interact $ simulate program
