

import Control.Concurrent (threadDelay)
import HasKAL.TimeUtils.GPSfunction (getCurrentLocalTime)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import System.Process (rawSystem)

main = do
  f <- getArgs >>= \args -> case (length args) of
    1 -> return (head args)
    _ -> error " Usage: runDailyFileTransfer f"
  go f


go f = do localTime' <- getCurrentLocalTime "JST"
          let localTime = take 10 localTime'
          putStrLn ("running daily file transfer"++localTime++".") >> hFlush stdout
          rawSystem f []
          threadDelay (24*3600*1000000)
          go f

