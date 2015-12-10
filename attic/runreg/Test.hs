
--module Test
--where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.DeepSeq (deepseq)
import Control.Monad (forever, mapM_)
import Control.Monad.State (liftIO)
import Data.Conduit (Conduit, Sink, Source, await, yield, ($$),($=))
import Data.List (elemIndices)
import Data.Text (pack)
import Filesystem.Path (extension, (</>))
import Filesystem.Path.CurrentOS (decodeString, encodeString)
import HasKAL.DataBaseUtils.FrameFull.DataBaseAdmin (updateFrameDB')
import System.Environment (getArgs)
import System.FSNotify (Debounce(..), Event(..), WatchConfig(..), withManagerConf, watchDir, watchTreeChan, watchDirChan, eventPath)
import System.IO (hFlush, stdout)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan
import System.FilePath.Posix (takeExtension, takeFileName)
import HasKAL.TimeUtils.GPSfunction (getCurrentGps)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.Timeout (timeout)
import System.IO.Unsafe (unsafePerformIO)



main = do
  topDir <- getArgs >>= \args -> case (length args) of
   1 -> return (head args)
   _ -> error "Usage : RunRegistFrame2FrameFullDB topdir"
  source topDir $$ sink


source :: FilePath
       -> Source IO FilePath
source topDir = do
  gps <- liftIO getCurrentGps
  liftIO $ putStrLn gps >> hFlush stdout
  let cdir = getCurrentDir gps
      cabspath = getAbsPath topDir cdir
  liftIO $ putStrLn cabspath >> hFlush stdout
  gowatch cabspath $ do watchFile topDir cdir
--                        liftIO $ threadDelay ((timeToNextDir gps+2)*1000000)
                        gps' <- liftIO getCurrentGps
                        liftIO $ putStrLn (gps'++": next loop") >> hFlush stdout
                        source topDir


watchFile :: FilePath
          -> FilePath
          -> Source IO FilePath
watchFile topDir watchdir = do
  liftIO $ putStrLn ("watchFile: start watchning "++watchdir++".") >> hFlush stdout
  let absPath = getAbsPath topDir watchdir
      predicate event' = case event' of
        Added path _ -> chekingFile path
        _            -> False
      config = WatchConfig
                 { confDebounce = NoDebounce
                 , confPollInterval = 1000000 -- 1seconds
                 , confUsePolling = True
                 }
  maybefname <- liftIO $ timeout (breakeTime 5) $ withManagerConf config $ \manager -> do
    fname <- liftIO newEmptyMVar
    watchDir manager absPath predicate
      $ \event -> case event of
        Added path _ -> putMVar fname path
    takeMVar fname
  case maybefname of
    Nothing       -> return ()
    Just gwfname  -> do liftIO $ putStrLn "gwf file found" >> hFlush stdout
                        yield gwfname >> watchFile topDir watchdir


sink :: Sink String IO ()
sink = do
  c <- await
  case c of
    Nothing -> do liftIO $ putStrLn "Nothing" >> hFlush stdout
                  sink
    Just fname -> do liftIO $ putStrLn fname >> hFlush stdout
--                     x <- liftIO $ updateFrameDB' fname
--                     liftIO $ x `deepseq` return ()
                     sink


chekingFile path = takeExtension path `elem` [".gwf"] && head (takeFileName path) /= '.'


getAbsPath dir1 dir2 = encodeString $ decodeString dir1 </> decodeString dir2


breakeTime margin = unsafePerformIO $ do
  dt <- getCurrentGps >>= \gps-> return $ timeToNextDir gps
  return $ (dt+margin)*1000000


num = 8


getCurrentDir :: String -> String
getCurrentDir gps = take num gps


getNextDir :: String -> String
getNextDir gps =
  let gpsHead' = take num gps
      gpsHead = read gpsHead' :: Int
   in take num $ show (gpsHead+1)


timeToNextDir :: String -> Int
timeToNextDir gps =
  let currentGps = read gps :: Int
      (gpsHead', gpsTail') = (take num gps, drop num gps)
      gpsHead = read gpsHead' :: Int
      gpsTail = replicate (length gpsTail') '0'
      nextGps = read (show (gpsHead+1) ++ gpsTail) :: Int
   in nextGps - currentGps


gowatch dname f =  do b <- liftIO $ doesDirectoryExist dname
                      case b of
                       False -> do liftIO $ threadDelay 1000000
                                   gowatch dname f
                       True  -> f













