


--module RegistDB
--where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.DeepSeq (deepseq)
import Control.Monad.State (liftIO)
import Data.Conduit (Sink, Source, await, yield, ($$))
import Data.List (elemIndices)
import Data.Text (pack)
import Filesystem.Path (extension)
import Filesystem.Path.CurrentOS (decodeString, encodeString)
import HasKAL.DataBaseUtils.FrameFull.DataBaseAdmin (updateFrameDB')
import System.Environment (getArgs)
import System.FSNotify (Debounce(..), Event(..), WatchConfig(..), withManagerConf, watchTree, eventPath)
import System.IO (hFlush, stdout)


--runRegistDB watchdir = source watchdir $$ sink

main = do
  dirname <- getArgs >>= \args -> case (length args) of
   1 -> return (head args)
   _ -> error "Usage : RunRegistFrame2FrameFullDB dir"
  source dirname $$ sink

source :: FilePath
       -> Source IO FilePath
source watchdir = do
  let config = WatchConfig
                 { confDebounce = DebounceDefault
                 , confPollInterval = 20000000 -- 20seconds
                 , confUsePolling = True
                 }
  x <- liftIO $ withManagerConf config $ \manager -> do
    fname <- liftIO newEmptyMVar
    _ <- watchTree manager watchdir (const True)
      $ \event -> case event of
        Removed _ _ -> putStrLn "file removed" >> hFlush stdout
        _           -> do let gwfname = eventPath event
                          case (length (elemIndices '.' gwfname)) of
                            1 -> putMVar fname gwfname
                            2 -> putStrLn "file saving" >> hFlush stdout
                            _ -> putStrLn "file extension should be .filepart or .gwf" >> hFlush stdout
    takeMVar fname
  yield x >> source watchdir
  where filepart = pack "filepart"
        gwf = pack "gwf"


sink :: Sink String IO ()
sink = do
  c <- await
  case c of
    Nothing -> do liftIO $ putStrLn "Nothing" >> hFlush stdout
                  sink
    Just fname -> do liftIO $ putStrLn fname >> hFlush stdout
                     x <- liftIO $ updateFrameDB' fname
                     liftIO $ x `deepseq` return ()
                     sink






