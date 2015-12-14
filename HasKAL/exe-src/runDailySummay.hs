

import Control.Concurrent (threadDelay)
import HasKAL.TimeUtils.GPSfunction (getCurrentLocalTime)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import System.Process (rawSystem)

main = do
  (f, opt) <- getArgs >>= \args -> case (length args) of
    2 -> return (head args, args!!1)
    _ -> error " Usage: runDailySummary f opt"
  go f opt


go f opt = do localTime' <- getCurrentLocalTime "JST"
              let localTime = take 10 localTime'
              putStrLn ("generating daily summary page for "++localTime ++".") >> hFlush stdout
              rawSystem f [opt]
              threadDelay (24*3600*1000000)
              go f opt

