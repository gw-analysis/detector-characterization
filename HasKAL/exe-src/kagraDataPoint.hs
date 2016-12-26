

import HasKAL.DataBaseUtils.FrameFull.Function (kagraDataPoint)
import System.Environment (getArgs)
import Data.Int (Int32)
import System.IO (stdout, hPutStrLn)

main = do
  (gpsstrt',chname) <- getArgs >>= \args -> case length args of
    2 -> return (head args, args!!1)
    _ -> error "Usage: kagraDataPoint gps_start[s] channel"
  let gpsstrt = read gpsstrt' :: Int32
  statement <- kagraDataPoint gpsstrt chname
  case statement of
    Nothing -> print "Nothing"
    Just x -> mapM_ (hPutStrLn stdout) x

