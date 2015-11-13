

import Data.List (sort,elemIndices)
import HasKAL.WebUtils.DailySummaryPage (genDailySummaryPage)
import System.Environment (getArgs)
import Data


main = do
  (dir, date, chlist, monlist, subsystem, ncol) <- getArgs >>= \args -> case (length args) of
    6 -> return (args!!0,args!!1,args!!2,args!!3,args!!4,args!!5)
    _ -> error "Usage: genDailySummaryPage dir date chlist monlist subsystem ncol"
  chs <- readFile chlist >>= \x -> return $ removeCommentLines (lines x)
  mons <- readFile monlist >>= \x -> return $ removeCommentLines (lines x)
  let mons' = map show $ sort $ map (\x->read x :: Monitor) (dealingwithLT mons)
  genDailySummaryPage dir date chs mons' subsystem (read ncol::Int)


removeCommentLines = concatMap (\x-> words $ take (head' x $ elemIndices '#' x) x)

head' y x | x ==[] = length y | otherwise = head x

dealingwithLT xs = ["LTF"|x<-xs,x=="LT"] ++ ["LTA"|x<-xs,x=="LT"] ++[x|x<-xs,x/="LT"]
