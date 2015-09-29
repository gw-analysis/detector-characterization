

import HasKAL.DataBaseUtils.Framedb (framedb)
import HasKAL.DataBaseUtils.Function (db2framecache)
import System.Environment (getArgs)
import System.IO (stdout, hPutStrLn)


main = do
  args <- getArgs
  let fileName | length args == 1 = args!!0
               | otherwise = error "usage : db2cache output_file"
  contents <- db2framecache framedb
  case contents of
    Nothing -> print "Nothing"
    Just x -> writeFile fileName $ unlines x
