

import Control.Concurrent (threadDelay)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stdout)
import System.Process (rawSystem)

main = do
  f <- getArgs >>= \args -> case (length args) of
    1 -> return (head args)
    _ -> error " Usage: runDailySummary f"
  go f


go f = do rawSystem f []
          threadDelay (24*3600*1000000)
          go f

