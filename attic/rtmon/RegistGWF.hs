

import HasKAL.DataBaseUtils.DataBaseAdmin(updateFrameDB)
import System.Environment (getArgs)

main = do
  args <- getArgs
  let fname = head args
  case length args of
    1 -> updateFrameDBfromcache fname
    _ -> error "Usage: registGWF framecache"
