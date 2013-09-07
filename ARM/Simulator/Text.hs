module ARM.Simulator.Text (simulate) where
  import ARM.Simulator.Common
  import Control.Concurrent.Async
  import Control.Monad.State
  import Data.Word
  import System.IO

  type TextWorld = World (Async Char)

  simulate :: [Word32] -> IO ()
  simulate program = 
    do hSetBuffering stdin NoBuffering
       hSetBuffering stdout NoBuffering
       term <- hIsTerminalDevice stdin
       when term $ hSetEcho stdin False
       bus <- makeBus program
       as <- async getChar
       evalStateT runWorld $ World True bus reset ("", "") as

  runWorld :: StateT TextWorld IO ()
  runWorld = do
    run <- gets running
    when run $ do performIO
                  busCycle
                  runWorld

  performIO :: StateT TextWorld IO ()
  performIO = do
    (input, output) <- gets busIO 
    output' <- if null output
                then return output
                else lift $ do putStr output; return ""
    as <- gets etc
    asyncResult <- lift $ poll as
    case asyncResult of
      Just (Right c) ->
        do as' <- lift $ async getChar
           modify $ \w -> w {etc = as', busIO = (input ++ [c], output')}
      Just (Left e) -> error $ show e
      Nothing -> modify $ \w -> w {busIO = (input, output')}
