
module HasKAL.MonitorUtils.GlitchMon.Env
(sqlite3dbfilename)
where

import HasKAL.DataBaseUtils.KAGRADataSource (defineTable)
import System.IO.Unsafe

import HasKAL.Misc.Environment(haskalOpt)

sqlite3dbfilename = unsafePerformIO $ do
  genvf <- readFile (haskalOpt++"/"++"glitchEnv.txt")
  return $ head . lines $ genvf
