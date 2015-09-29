

import System.Environment (getArgs)
import HasKAL.DataBaseUtils.DataBaseAdmin (updateFrameDBfromcache)

main = do
  args <- getArgs 
  fname <- case (length args) of
            1 -> return $ args!!0
            _ -> error "Usage: updateDBfromCache cachefile"

  updateFrameDBfromcache fname
  
