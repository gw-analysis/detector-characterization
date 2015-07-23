{-# LANGUAGE OverloadedStrings #-} 

module HasKAL.WebUtils.FileWatcher 
( watchNewfile
, watchNewfilewDB
) where

import Control.Concurrent
import Filesystem (getWorkingDirectory)
import System.FSNotify
import System.Exit
import System.IO.Error
import Filesystem.Path
import Filesystem.Path.CurrentOS (decodeString, encodeString)
--import HasKAL.WebUtils.EventDisplayXend
--import HasKAL.WebUtils.Data
import System.Process (rawSystem)
import Data.Text

watchNewfile :: String            -- | executive filename
             -> String            -- | location where index.html will be
             -> String            -- | location to watch new file added
             -> IO ()
watchNewfile f webhomedir watchdir = do
--  dir <- getWorkingDirectory
  withManager $ \manager -> do
    watchDir manager (decodeString watchdir) (const True)
      $ \event -> case event of
        Removed _ _ -> print "file removed"
        _ -> case extension (eventPath event) of
               Just "filepart" -> print "file downloading"
               Just "gwf" -> do
                 let gwfname = encodeString $ eventPath event
                 print gwfname
                 rawSystem f [webhomedir, gwfname]
                 print "event display updated."
               Nothing -> print "file extension should be .filepart or .gwf"
--      $ \event -> do rawSystem f [webhomedir, "/home/detchar/xend/K-K1_R-1113209036-32.gwf"]
--      $ \event -> channelPlot' param (encodeString $ eventPath event) >>= genWebPage'
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
    watchDir manager (decodeString watchdir) (const True)
      $ \event -> case event of
        Removed _ _ -> print "file removed"
        _ -> case extension (eventPath event) of
               Just "filepart" -> print "file downloading"
               Just "gwf" -> do
                 let gwfname = encodeString $ eventPath event
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


