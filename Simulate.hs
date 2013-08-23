module Main where
  import ARM.Simulator
  main = do
    let program = [0xE280000A, 0xE2400001, 0xEAFFFFF8]
    interact $ simulate program
