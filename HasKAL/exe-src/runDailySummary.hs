

import Control.Concurrent (threadDelay)
import HasKAL.TimeUtils.GPSfunction (getCurrentLocalTime)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import System.Process (rawSystem)
import Data.Time.Clock (diffUTCTime,getCurrentTime)

main = do
  (f, opt, waittime) <- getArgs >>= \args -> case (length args) of
    3 -> return (head args, args!!1, args!!2)
    _ -> error " Usage: runDailySummary f opt waitTime"
  let waittime' = read waittime :: Int
  threadDelay $ (waittime'+600)*1000000 
  go f opt


go f opt = do localTime' <- getCurrentLocalTime "JST"
              t0 <- getCurrentTime
              let localTime = take 10 localTime'
              putStrLn ("generating daily summary page for "++localTime ++".") >> hFlush stdout
              rawSystem f [opt]
              t1 <- getCurrentTime
              let dt = diffUTCTime t1 t0
                  offset = read (init $ show dt) :: Double
              threadDelay (24*3600*1000000 - (floor offset)*1000000)
              go f opt

