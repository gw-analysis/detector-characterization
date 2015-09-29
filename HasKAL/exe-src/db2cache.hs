

import HasKAL.DataBaseUtils.Function (db2framecache)
import System.Environment (getArgs)
import System.IO (stdout, hPutStrLn)

main = do
  args <- getArgs
  let (dbname, fileName) | length args == 2 = (args!0, args!1)
                         | otherwise = error "usage : db2cache database_name output_file"
  contents <- db2framecache dbname
  case contents of
    Nothing -> print "Nothing"
    Just x -> writeFile fileName $ unlines x
