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
                     ,busIO :: (String, String)
                     }

  simulate :: [Word32] -> IO ()
  simulate program = do
    bus <- makeBus program
    let machine = reset
    playIO (InWindow "ARM" (1024,768) (100, 100))
           white
           600
           (World False 3.0 0.0 0 bus machine ("", ""))
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
  handleEvent (EventKey (Char c) Up _ _) w = execStateT (worldInput c) w
  handleEvent (EventKey (SpecialKey KeySpace) Up _ _) w = execStateT (worldInput ' ') w
  handleEvent (EventKey (SpecialKey KeyEnter) Up _ _) w = execStateT (worldInput '\n') w
  handleEvent _ w = return w

  worldInput :: Char -> StateT World IO ()
  worldInput c = do
    (input, output) <- gets busIO
    modify $ \w -> w {busIO = (input ++ [c], output)}

  draw :: World -> IO Picture
  draw (World _ _ _ t b m (_, o)) = return $ 
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

  worldRead :: BusAddress -> MemSize -> StateT World IO Word32
  worldRead addr sz = do
    bus <- gets bus
    busIO <- gets busIO
    ((val, bus'), busIO') <- lift $ flip runStateT busIO $ busRead bus addr sz
    modify $ \w -> w {bus = bus', busIO = busIO'}
    return val

  worldWrite :: BusAddress -> MemSize -> Word32 -> StateT World IO ()
  worldWrite addr sz val = do
    bus <- gets bus
    busIO <- gets busIO
    (bus', busIO') <- lift $ flip runStateT busIO $ busWrite bus addr sz val
    modify $ \w -> w {bus = bus', busIO = busIO'}

  fetchAndStep :: StateT World IO Action
  fetchAndStep = do
    machine <- gets machine
    let pc = flip evalState machine $ getRegister R15
    ir <- worldRead pc WORD
    let (action, machine') = 
          flip runState machine $ do modify $ \s -> s {ir = ir}
                                     setRegister R15 (pc + 4)
                                     step
    modify $ \w -> w {machine = machine'}
    return action

  execRead :: Register -> BusAddress -> MemSize -> Signedness ->
             StateT World IO ()
  execRead rd addr sz sg =
    do val <- worldRead addr sz
       let bits = case sz of WORD -> 0; HALF -> 16; BYTE -> 24
       let val' = if sg then dropBits bits val
                       else let sVal = fromIntegral val :: Int32 
                            in fromIntegral $ dropBits bits sVal
       machine <- gets machine
       let machine' = flip execState machine $ setRegister rd val'
       modify $ \w -> w {machine = machine'}
       return ()
    where dropBits :: (Bits a) => Int -> a -> a
          dropBits sh val = (val `shiftL` sh) `shiftR` sh

  execWrite :: Register -> BusAddress -> MemSize -> StateT World IO ()
  execWrite rd addr sz = do
    machine <- gets machine
    let val = flip evalState machine $ getRegister rd 
    worldWrite addr sz val

  busCycle :: World -> IO World
  busCycle = execStateT $ do
    action <- fetchAndStep
    case action of
      Stop -> modify $ \w -> w {running = False}
      Continue -> return ()
      ReadMem rd addr sz sg -> execRead rd addr sz sg
      WriteMem rd addr sz -> execWrite rd addr sz
