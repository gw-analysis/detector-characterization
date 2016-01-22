

import HasKAL.DataBaseUtils.FrameFull.Function (kagraDailyFileList)
import System.Environment (getArgs)
import System.IO (stdout,  hPutStrLn)

main = do
  args <- getArgs
  case length args of
    2 -> do let day = head args 
                loc = args !! 1
            statement <- kagraDailyFileList day loc
            case statement of
              Nothing -> print "Nothing"
              Just x -> mapM_ (hPutStrLn stdout) x
    _ -> error "Usage: kagraDailyDataFind day(yyyy-mm-dd) localtime(JST,UTC,etc)"




