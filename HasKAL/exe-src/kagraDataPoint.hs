

import HasKAL.DataBaseUtils.Function (kagraDataPoint)
import System.Environment (getArgs)
import Data.Int (Int32)
import System.IO (stdout, hPutStrLn)

main = do
  args <- getArgs
  let gpsstrt = read (head args) :: Int32
      chname = args !! 1 :: String
  statement <- kagraDataPoint gpsstrt chname
  case statement of
    Nothing -> print "Nothing"
    Just x -> mapM_ (hPutStrLn stdout) x

