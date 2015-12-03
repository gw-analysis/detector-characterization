


--module RegistDB
--where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.DeepSeq (deepseq)
import Control.Monad (forever)
import Control.Monad.State (liftIO)
import Data.Conduit (Sink, Source, await, yield, ($$))
import Data.List (elemIndices)
import Data.Text (pack)
import Filesystem.Path (extension, (</>))
import Filesystem.Path.CurrentOS (decodeString, encodeString)
import HasKAL.DataBaseUtils.FrameFull.DataBaseAdmin (updateFrameDB')
import System.Environment (getArgs)
import System.FSNotify (Debounce(..), Event(..), WatchConfig(..), withManagerConf, watchTree, watchTreeChan, eventPath)
import System.IO (hFlush, stdout)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan
import System.FilePath.Posix (takeExtension, takeFileName)
import HasKAL.TimeUtils.GPSfunction (getCurrentGps)
--runRegistDB watchdir = source watchdir $$ sink

main = do
  dirname <- getArgs >>= \args -> case (length args) of
   1 -> return (head args)
   _ -> error "Usage : RunRegistFrame2FrameFullDB dir"
  source dirname $$ sink


source :: FilePath
       -> Source IO FilePath
source watchTopdir = do
  let predicate event' = case event' of
        Added path _ -> head (takeFileName path) /= '.'
        _            -> False
  let config = WatchConfig
                 { confDebounce = NoDebounce
                 , confPollInterval = 5000000 -- 5seconds
                 , confUsePolling = True
                 }
  x <- liftIO $ withManagerConf config $ \manager -> do
    fname <- liftIO newEmptyMVar
    dirname <- getCurrentGps >>= \x -> return $ take 5 (show ((read x ::Int)-10))
    _ <- watchTree manager (encodeString $ decodeString watchTopdir </> decodeString dirname) predicate
      $ \event -> case event of
        Added path _ -> case (takeExtension path `elem` [".gwf"] && head (takeFileName path) /= '.') of
                          True  -> putMVar fname path
                          False -> putStrLn "something strange happens." >> hFlush stdout
    takeMVar fname
  liftIO $ putStrLn x >> hFlush stdout
  yield x >> source watchTopdir


source' :: FilePath
       -> Source IO FilePath
source' watchdir = do
  let predicate event' = case event' of
        Added path _ -> takeExtension path `elem` [".gwf"] && head (takeFileName path) /= '.'
        _            -> False
  eventChan <- liftIO newChan
  let config = WatchConfig
                 { confDebounce = NoDebounce
                 , confPollInterval = 5000000 -- 5seconds
                 , confUsePolling = True
                 }
  _ <- liftIO $ forkIO $ withManagerConf config $ \manager -> do
    _<-watchTreeChan 
       manager 
       watchdir 
       predicate
       eventChan
--    return ()
    forever $ threadDelay 10000000
  event <- liftIO $  readChan eventChan 
  let gwfname = eventPath event
  _<- liftIO $ putStrLn gwfname >> hFlush stdout 
  source watchdir
--  yield x >> source watchdir


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






