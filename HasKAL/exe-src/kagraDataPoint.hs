

import HasKAL.DataBaseUtils.Function (kagraDataPoint)
import System.Environment (getArgs)
import Data.Int (Int32)

main = do
  args <- getArgs
  let gpsstrt = read (head args) :: Int32
      chname = args !! 1 :: String
  statement <- kagraDataPoint gpsstrt chname
  print statement


