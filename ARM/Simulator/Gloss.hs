module ARM.Simulator.Gloss (ARM.Simulator.Gloss.simulate) where

  import ARM.InstructionSet as I
  import ARM.Disassembler
  import ARM.Simulator.Common
  import Control.Monad
  import Control.Monad.State
  import Data.Array
  import Data.Array.MArray
  import Data.Array.IO
  import Data.Bits
  import Data.Int
  import Data.Word
  import Graphics.Gloss
  import Graphics.Gloss.Interface.IO.Game
  import Text.Printf

  import Debug.Trace

  data World = World {running :: Bool
                     ,initDelay :: Float
                     ,delay :: Float
                     ,tsc :: Integer
                     ,bus :: Bus
                     ,machine :: Machine
                     ,input :: String
                     ,output :: String
                     }

  simulate :: [Word32] -> IO ()
  simulate program = do
    bus <- makeBus program
    let machine = reset
    playIO (InWindow "ARM" (1024,768) (100, 100))
           white
           60
           (World False 3.0 0.0 0 bus machine [] [])
           draw
           handleEvent
           (\t w -> if running w
                      then let w' = w { delay = delay w - t }
                           in if delay w' < 0 
                                then busCycle $ w' { delay = initDelay w }
                                else return w'
                      else return w)

  handleEvent :: Event -> World -> IO World
  handleEvent (EventKey (SpecialKey KeyF1) Up _ _) w = 
    return w {running = not $ running w}
  handleEvent (EventKey (SpecialKey KeyF2) Up _ _) w = return w {initDelay = initDelay w / 2 }
  handleEvent (EventKey (SpecialKey KeyF3) Up _ _) w = return w {initDelay = initDelay w * 2}
  handleEvent (EventKey (SpecialKey KeyF8) Up _ _) w = busCycle w
  handleEvent (EventKey (Char c) Up _ _) w = return w {input = input w ++ [c]}
  handleEvent (EventKey (SpecialKey KeySpace) Up _ _) w = return w {input = input w ++ " "}
  handleEvent (EventKey (SpecialKey KeyEnter) Up _ _) w = return w {input = input w ++ "\n"}
  handleEvent _ w = return w

  busCycle :: World -> IO World
  busCycle w = do
    do let tsc' = tsc w + 1
       let (action, machine') = runState step $ machine w
       let pc = evalState (getRegister R15) machine'
       (ir, bus', input', output') <- busRead (bus w) pc WORD (input w) (output w)
       let machine'' = flip execState machine' $ do modify $ \s -> s {ir = ir}
                                                    setRegister R15 (pc + 4)
       case action of
         Stop -> return $ w {running = False, tsc = tsc'
                            ,bus = bus', machine = machine''}
         Continue -> return $ w {tsc = tsc', bus = bus', machine = machine''}
         ReadMem rd addr sz sg ->
           do (val, bus'', input'', output'') <- busRead bus' addr sz input' output'
              let bits = case sz of WORD -> 0; HALF -> 16; BYTE -> 24
              let val' = if sg then dropBits bits val
                               else let sVal = fromIntegral val :: Int32 
                                    in fromIntegral $ dropBits bits sVal
              let machine''' = flip execState machine'' $ setRegister rd val'
              return $ w {tsc = tsc', bus = bus'', machine = machine'''
                         ,input = input'', output = output''}
         WriteMem rd addr sz -> 
           do let val = flip evalState machine'' $ getRegister rd 
              (bus'', input', output') <- busWrite bus' addr sz val (input w) (output w)
              return $ w {tsc = tsc', bus = bus'', machine = machine''
                         ,input = input', output = output'}
    where dropBits :: (Bits a) => Int -> a -> a
          dropBits sh val = (val `shiftL` sh) `shiftR` sh

  draw :: World -> IO Picture
  draw (World _ _ _ t b m i o) = return $ 
    pictures [translate (-500.0) 350.0 $ color orange $ drawShow t
             ,translate (-500.0) 300.0 $ color green $ drawRF m 
             ,translate (-250.0) 300.0 $ color blue $ drawShow $ disassembleI $ ir m
             ,translate (-500.0) (-100.0) $ color black $ drawOutput o
             ]

  drawOutput :: String -> Picture
  drawOutput o = translate 200.0 0.0 $ pictures $ [label, output]
    where label = translate (-200.0) 0.0 $ scale 0.3 0.3 $ text $ "output: "
          output = pictures $ zipWith (translate 0.0) [0,-60.0..] outputLines
          outputLines = map (scale 0.3 0.3 . text) $ lines o

  drawShow :: Show a => a -> Picture
  drawShow s = scale 0.2 0.2 $ text $ show s

  drawRF :: Machine -> Picture
  drawRF m = pictures $ map (uncurry drawReg) $ assocs $ rf m
    where drawReg :: Register -> Word32 -> Picture
          drawReg r w = translate 0.0 (fromIntegral (fromEnum r) * (-20.0)) 
                        $ scale 0.1 0.1
                        $ color black 
                        $ pictures [text $ show r
                                   ,translate 400.0 0 $ text $ printf "%08x" w]
