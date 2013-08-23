module Main where
  import ARM.Simulator
  import ARM.Assembler
  import ARM.Parser
  import Data.List
  import System.Environment
  import System.IO

  main = do
    (input:_) <- getArgs
    let (Just baseNameRev) = stripPrefix "s." . reverse $ input
        output = reverse ("tuo." ++ baseNameRev)
    contents <- readFile input
    let instructions = parseARM contents
        program = assemble instructions
    interact $ simulate program
