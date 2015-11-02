

import HasKAL.WebUtils.DailySummaryPage (genDailySummaryPage)
import System.Environment (getArgs)

main = do
  (dir, date, chlist, monlist, subsystem, ncol) <- getArgs >>= \args -> case (length args) of
    6 -> return (args!!0,args!!1,args!!2,args!!3,args!!4,args!!5)
    _ -> error "Usage: genDailySummaryPage dir date chlist monlist subsystem ncol"
  genDailySummaryPage dir date chlist monlist subsystem (read ncol::Int)


