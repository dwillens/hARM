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

  data GlossEtc = GlossEtc {initDelay :: Float
                           ,delay :: Float
                           ,tsc :: Integer
                           }
  type GlossWorld = World GlossEtc

  simulate :: [Word32] -> IO ()
  simulate program = do
    bus <- makeBus program
    let machine = reset
    playIO (InWindow "ARM" (1024,768) (100, 100))
           white
           600
           (World False bus machine ("", "") (GlossEtc 3.0 0.0 0))
           draw
           handleEvent
           handleFrame

  handleFrame :: Float -> GlossWorld -> IO GlossWorld
  handleFrame t = execStateT $ do
    running <- gets running
    when running $ modify $ \w -> let e = etc w 
                                  in w {etc = e {delay = delay e - t}}
    delay <- liftM delay $ gets etc
    when (running && delay < 0) $ do 
      modify $ \w -> let e = etc w in w {etc = e {delay = initDelay e}}
      busCycle

  handleEvent :: Event -> GlossWorld -> IO GlossWorld
  handleEvent (EventKey k Up _ _) = execStateT $ handleKey k
  handleEvent _ = return 

  handleKey :: Key -> StateT GlossWorld IO ()
  handleKey (SpecialKey KeyF1) = modify $ \w -> w {running = not $ running w}
  handleKey (SpecialKey KeyF2) = modify $ \w -> 
    let e = etc w in w {etc = e {initDelay = initDelay e / 2}}
  handleKey (SpecialKey KeyF3) = modify $ \w -> 
    let e = etc w in w {etc = e {initDelay = initDelay e * 2}}
  handleKey (SpecialKey KeyF8) = busCycle
  handleKey (Char c) = worldInput c
  handleKey (SpecialKey KeySpace) = worldInput ' '
  handleKey (SpecialKey KeyEnter) = worldInput '\n'
  handleKey _ = return ()

  worldInput :: Char -> StateT GlossWorld IO ()
  worldInput c = do
    (input, output) <- gets busIO
    modify $ \w -> w {busIO = (input ++ [c], output)}

  draw :: GlossWorld -> IO Picture
  draw (World _ b m (_, o) (GlossEtc _ _ t)) = return $ 
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

