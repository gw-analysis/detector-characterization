

import HasKAL.DataBaseUtils.FrameFull.Function (kagraDataFind)
import System.Environment (getArgs)
import Data.Int (Int32)
import System.IO (stdout,  hPutStrLn)

main = do
  (gpsstrt',duration',chname) <- getArgs >>= \args -> case length args of
    3 -> return (head args, args!!1, args!!2)
    _ -> error "Usage: kagraDataFind gps_start[s] duration[s] channel"
  let gpsstrt = read gpsstrt' :: Int32
      duration = read duration' :: Int32
  statement <- kagraDataFind gpsstrt duration chname
  case statement of
    Nothing -> print "Nothing"
    Just x -> mapM_ (hPutStrLn stdout) x





