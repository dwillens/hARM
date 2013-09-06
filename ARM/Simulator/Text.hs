module ARM.Simulator.Text (simulate) where
  import ARM.Simulator.Common
  import Control.Concurrent.Async
  import Control.Monad.State
  import Data.Word
  import System.IO

  type TextWorld = World (Async Char)

  memSize :: (Integral a) => a
  memSize = 0x200000

  simulate :: [Word32] -> IO ()
  simulate program = 
    do hSetBuffering stdin NoBuffering
       hSetBuffering stdout NoBuffering
       term <- hIsTerminalDevice stdin
       when term $ hSetEcho stdin False
       bus <- makeBus program
       as <- async getChar
       runWorld $ World True bus reset ("", "") as

  runWorld :: TextWorld -> IO ()
  runWorld w = if running w 
                then do w' <- performIO w 
                        w'' <- execStateT busCycle w'
                        runWorld w''
                else return ()

  performIO :: TextWorld -> IO TextWorld
  performIO w@(World _ _ _ (input, output) as) =
    do output' <- if null output 
                    then return output
                    else do putStr output; return ""
       asyncResult <- poll as
       case asyncResult of
            Just (Right c) ->
              do as' <- async getChar
                 return w {etc = as', busIO = (input ++ [c], output')}
            Just (Left e) -> error $ show e
            Nothing -> return w {busIO = (input, output')}
