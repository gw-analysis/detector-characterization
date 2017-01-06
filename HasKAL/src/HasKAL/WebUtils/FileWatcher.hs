{-# LANGUAGE OverloadedStrings #-}

module HasKAL.WebUtils.FileWatcher
( hrsync
, watchNewfile
, watchNewfilewDB
, updatingDB
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Chan (newChan, readChan)
import Control.Monad (forever)
import Control.Monad.State (liftIO)
import System.Exit
import System.IO.Error
import Filesystem.Path
import Filesystem.Path.CurrentOS (decodeString, encodeString)
import System.FilePath.Posix (takeFileName)
import System.FSNotify ( Debounce(..)
                       , Event(..)
                       , WatchConfig(..)
                       , withManager
                       , withManagerConf
                       , watchDir
                       , watchTreeChan
                       , watchDirChan
                       , eventPath
                       )

import System.Process (rawSystem)
import Data.Text hiding (head)


hrsync :: String            -- | executive filename
       -> String            -- | dir to watch
       -> String            -- | dir to save
       -> IO ExitCode
hrsync f from to = do
  let predicate event' = case event' of
        Added path _ -> chekingFile path
        _            -> False
        where
         chekingFile path = head (takeFileName path) /= '.'
      config = WatchConfig
                 { confDebounce = NoDebounce
                 , confPollInterval = 5000000 -- 5seconds
                 , confUsePolling = True
                 }
  eventChan <- liftIO newChan
  withManagerConf config $ \manager -> do
    watchTreeChan
      manager
      from
      predicate
      eventChan
    forever $ threadDelay 10000000
  event <- liftIO $ readChan eventChan
  let fname = eventPath event
  rawSystem f [to, fname]




watchNewfile :: String            -- | executive filename
             -> String            -- | location where index.html will be
             -> String            -- | location to watch new file added
             -> IO ()
watchNewfile f webhomedir watchdir = do
  let predicate event' = case event' of
        Added path _ -> chekingFile path
        _            -> False
        where
         chekingFile path = head (takeFileName path) /= '.' 
                            && (extension (decodeString path)) == Just (pack "gwf")
      config = WatchConfig
                 { confDebounce = NoDebounce
                 , confPollInterval = 5000000 -- 5seconds
                 , confUsePolling = True
                 }
  withManagerConf config $ \manager -> do
    watchDir manager watchdir predicate
      $ \event -> do let gwfname = eventPath event
                     rawSystem f [webhomedir, gwfname] >> return ()
    waitBreak
  where
    waitBreak = do
      _ <- catchIOError getLine (\e -> if isEOFError e then exitSuccess else exitFailure)
      waitBreak


watchNewfilewDB :: String            -- | command name
                -> String            -- | DB command name
                -> String            -- | location where index.html will be
                -> String            -- | location to watch new file added
                -> IO ()
watchNewfilewDB f g webhomedir watchdir = do
  withManager $ \manager -> do
    watchDir manager watchdir (const True)
      $ \event -> case event of
        Removed _ _ -> print "file removed"
        _ -> case extension (decodeString (eventPath event)) of
               Just "filepart" -> print "file downloading"
               Just "gwf" -> do
                 let gwfname = eventPath event
                 print gwfname
                 rawSystem f [webhomedir, gwfname]
                 print "event display updated."
                 rawSystem g [gwfname]
                 print "database updated."

               Nothing -> print "file extension should be .filepart or .gwf"
    waitBreak
  where
    waitBreak = do
      _ <- catchIOError getLine (\e -> if isEOFError e then exitSuccess else exitFailure)
      waitBreak


updatingDB :: String            -- | DB command
           -> String            -- | location to watch new file added
           -> IO ()
updatingDB f watchdir = do
  withManager $ \manager -> do
    watchDir manager watchdir (const True)
      $ \event -> case event of
        Removed _ _ -> print "file removed"
        _ -> case extension (decodeString (eventPath event)) of
               Just "filepart" -> print "file downloading"
               Just "gwf" -> do
                 let gwfname = eventPath event
                 print gwfname
                 rawSystem f [gwfname]
                 print "framefile database updated."
               Nothing -> print "file extension should be .filepart or .gwf"
--      $ \event -> do rawSystem f [webhomedir, "/home/detchar/xend/K-K1_R-1113209036-32.gwf"]
--      $ \event -> channelPlot' param (encodeString $ eventPath event) >>= genWebPage'
    waitBreak
  where
    waitBreak = do
      _ <- catchIOError getLine (\e -> if isEOFError e then exitSuccess else exitFailure)
      waitBreak


