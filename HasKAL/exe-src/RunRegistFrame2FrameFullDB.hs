{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, BangPatterns #-}


--module RegistDB
--where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.DeepSeq (NFData, deepseq)
import Control.Monad (forever, mapM_)
import Control.Monad.State (liftIO)
import Data.Conduit ( Conduit
                    , Sink
                    , Source
                    , await
                    , runConduit
                    , yield
                    , ($$)
                    , ($=)
                    )
import Data.List (elemIndices)
import Data.Text (pack)
import Filesystem.Path (extension, (</>))
import Filesystem.Path.CurrentOS (decodeString, encodeString)
import HasKAL.DataBaseUtils.FrameFull.DataBaseAdmin (updateFrameDB')
import System.Environment (getArgs)
import System.FSNotify ( Debounce(..)
                       , Event(..)
                       , WatchConfig(..)
                       , withManagerConf
                       , watchDir
                       , watchTreeChan
                       , watchDirChan
                       , eventPath
                       )
import System.IO (hFlush, stdout)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan
import System.FilePath.Posix (takeExtension, takeFileName)
import HasKAL.TimeUtils.GPSfunction (getCurrentGps)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.Timeout.Lifted (timeout)
import System.IO.Unsafe (unsafePerformIO)


main = do
  topDir <- getArgs >>= \args -> case (length args) of
   1 -> return (head args)
   _ -> error "Usage : RunRegistFrame2FrameFullDB topdir"
  gps <- liftIO getCurrentGps
  let cdir = getCurrentDir gps
  _ <- source topDir cdir 
  return ()

source :: FilePath
       -> FilePath
       -> IO FilePath
source topDir watchdir = do  
  gps <- liftIO getCurrentGps
  let ndir = getNextDir gps
      ndirabs = getAbsPath topDir ndir
--  liftIO $ putStrLn ("start watching "++watchdir++".") >> hFlush stdout
  x <- doesDirectoryExist (getAbsPath topDir watchdir)
  case x of
    False -> do threadDelay 1000000
                gps2 <- getCurrentGps
                let cdir2 = getCurrentDir gps2
                source topDir cdir2
    True -> do !maybeT <- timeout (breakTime 20) $ watchFile topDir watchdir $$ sink
               case maybeT of
                Nothing -> do putStrLn ("Watching Timeout: going to next dir "++ndir++" to watch.") >> hFlush stdout
                              gowatch ndirabs (source topDir ndir) (source topDir)
                Just _  -> do putStrLn ("going to next dir "++ndir++" to watch.") >> hFlush stdout
                              gowatch ndirabs (source topDir ndir) (source topDir)



watchFile :: FilePath
          -> FilePath
          -> Source IO FilePath
watchFile topDir watchdir = do
  let absPath = getAbsPath topDir watchdir
      predicate event' = case event' of
        Added path _ -> chekingFile path 
        _            -> False
      config = WatchConfig
                 { confDebounce = NoDebounce 
                 , confPollInterval = 1000000 -- 1s
                 , confUsePolling = True
                 }
  gwfname <- liftIO $ withManagerConf config $ \manager -> do
    fname <- newEmptyMVar
    watchDir manager absPath predicate
      $ \event -> case event of
        Added path _ -> putMVar fname path
    takeMVar fname
  yield gwfname >> watchFile topDir watchdir


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


chekingFile path = takeExtension path `elem` [".gwf"] && head (takeFileName path) /= '.'


getAbsPath dir1 dir2 = encodeString $ decodeString dir1 </> decodeString dir2


breakTime margin = unsafePerformIO $ do
  dt <- getCurrentGps >>= \gps-> return $ timeToNextDir gps
  return $ (dt+margin)*1000000


getCurrentDir :: String -> String
getCurrentDir gps = take 5 gps


getNextDir :: String -> String
getNextDir gps = 
  let gpsHead' = take 5 gps
      gpsHead = read gpsHead' :: Int
   in take 5 $ show (gpsHead+1) 
 

timeToNextDir :: String -> Int
timeToNextDir gps = 
  let currentGps = read gps :: Int
      (gpsHead', gpsTail') = (take 5 gps, drop 5 gps)
      gpsHead = read gpsHead' :: Int
      gpsTail = replicate (length gpsTail') '0'
      nextGps = read (show (gpsHead+1) ++ gpsTail) :: Int
   in nextGps - currentGps


gowatch dname f g =  do b <- liftIO $ doesDirectoryExist dname
                        case b of
                          False -> do gps <- liftIO getCurrentGps
                                      let cdir' = getCurrentDir gps
                                          cdir  = drop (length dname -5) dname
                                      case cdir' > cdir of
                                        True -> do let dname' = (take (length dname -5) dname)++cdir'
                                                   liftIO $ threadDelay 1000000
                                                   gowatch dname' (g cdir') g
                                        False -> do liftIO $ threadDelay 1000000
                                                    gowatch dname f g
                          True  -> f
--                        True  -> do flist <- liftIO $ getDirectoryContents dname >>= \x-> 
--                                      return $ filter (\y-> not (y `elem` [".",".."]) && head y /='.') x
--                                    case null flist of
--                                      True -> f
--                                      False -> do mapM_ yield flist
--                                                  f


source' :: FilePath
        -> FilePath
        -> Source IO FilePath
source' topDir dirname = do
  let watchdir = encodeString $ decodeString topDir </> decodeString dirname
      predicate event' = case event' of
        Added path _ -> takeExtension path `elem` [".gwf"] && head (takeFileName path) /= '.'
        _            -> False
  eventChan <- liftIO newChan
  let config = WatchConfig
                 { confDebounce = NoDebounce
                 , confPollInterval = 5000000 -- 5seconds
                 , confUsePolling = True
                 }
  _ <- liftIO $ forkIO $ withManagerConf config $ \manager -> do
    _ <- watchDirChan 
         manager 
         watchdir 
         predicate
         eventChan
    return ()
--    forever $ threadDelay 10000000
  event <- liftIO $  readChan eventChan 
  let gwfname = eventPath event
  _<- liftIO $ putStrLn gwfname >> hFlush stdout 
--  source watchdir
  dirname' <- liftIO $ getCurrentGps >>= \x -> return $ take 5 (show ((read x ::Int)-10))
--  yield gwfname >> source' topDir dirname'
  source' topDir dirname'







