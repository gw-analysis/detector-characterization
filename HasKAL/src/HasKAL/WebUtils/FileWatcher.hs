
module HasKAL.WebUtils.FileWatcher (watchNewfile) where

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

watchNewfile :: String -> String -> String -> IO ()
watchNewfile f webhomedir watchdir = do
--  dir <- getWorkingDirectory
  withManager $ \manager -> do
    watchDir manager (decodeString watchdir) (const True)
      $ \event -> do rawSystem f [webhomedir, encodeString $ eventPath event]
                     print "event display updated."
--      $ \event -> channelPlot' param (encodeString $ eventPath event) >>= genWebPage'

    waitBreak
  where
    waitBreak = do
      _ <- catchIOError getLine (\e -> if isEOFError e then exitSuccess else exitFailure)
      waitBreak

