

import System.Environment (getArgs)
import HasKAL.DataBaseUtils.FrameFull.DataBaseAdmin (updateFrameDB')

main = do
  args <- getArgs 
  fname <- case (length args) of
            1 -> return $ head args
            _ -> error "Usage: updateFrameFullDB framefile"

  updateFrameDB' fname
  
