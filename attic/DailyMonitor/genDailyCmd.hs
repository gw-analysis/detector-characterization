

import System.Environment (getArgs)
import Control.Monad (liftM, forM)
import Data.List ((\\), nub, foldl', delete)

type ChName = String
type MonName = String

------------------------------------------------------------------------------
--  Filter rule
------------------------------------------------------------------------------
filterRule :: [(MonName, ChName)] -> [(MonName, ChName)]
filterRule = myfilter.nub
  where 
    -- enabled filters
    myfilter = rangeMonOnly . stVectorOnly

    -- each filters
    rangeMonOnly = limitChannels "dailyRangeMon" ["K1:GW-Channel"]
    stVectorOnly = limitChannels "dailySTVectorMon" ["K1:StVec1", "K1:StVec2"]


{-- Sample Filter 
    example1 = limitChannels "K1:Hoge" ["dailyHogeMon"]
                   -- When channel is K1:Hoge, only dailyHogeMon is applied.
                   -- When channel is another one, whole of monitors are applied.

    example2 = limitMonitors "dailyFugaMon" ["K1:Fuga"]
                   -- When monitor is dailyHoge, only K1:Fuga is analyzed.
                   -- When mhannel is another one, whole of channels are analyzed.
--}
------------------------------------------------------------------------------
--  End of filter function
------------------------------------------------------------------------------




{-- main function --}
main :: IO [()]
main = do
  {-- arguments --}
  args <- getArgs 
  (masterFile, yyyy, mm, dd) <-
    case length args of
     4 -> return $ (args!!0, (show0 4 $ args!!1), (show0 2 $ args!!2), (show0 2 $ args!!3))
     _ -> error "Usage: genDailyCmd master.lst yyyy mm dd"

  {-- read master file --}
  flist <- liftM (map words.lines) $ readFile masterFile

  {-- read each file --}
  chmonlst <- liftM concat $ forM flist $ \[chfile, monfile] -> do
    monlst <- liftM lines $ readFile monfile :: IO [MonName]
    chlst <- liftM lines $ readFile chfile :: IO [ChName]
    return $ exactaBox monlst chlst :: IO [(MonName, ChName)]

  {--  filter  --}
  let chmonlst' = filterRule chmonlst

  {-- chekck  --}
  forM chmonlst' $ \(mon, ch) -> do
    putStrLn $ mon ++++ yyyy ++++ mm ++++ dd ++++ ch

{-- internal functions --}
limitMonitors :: ChName -> [MonName] -> [(MonName, ChName)] -> [(MonName, ChName)]
limitMonitors ch mons orig = multiDelete del orig
  where del = multiDelete (map (flip exacta ch) mons) $ filter ((==ch).snd) orig


limitChannels :: MonName -> [ChName] -> [(MonName, ChName)] -> [(MonName, ChName)]
limitChannels mon chs orig = multiDelete del orig
  where del = multiDelete (map (exacta mon) chs) $ filter ((==mon).fst) orig


        
multiDelete :: [(MonName, ChName)] -> [(MonName, ChName)] -> [(MonName, ChName)]
multiDelete del orig = foldl' (\bs a -> delete a bs) orig del

exactaBox :: [MonName] -> [ChName] -> [(MonName,ChName)]
exactaBox xs ys = concat $ map (flip exactaWheel ys) xs

exactaWheel :: MonName -> [ChName] -> [(MonName,ChName)]
exactaWheel x ys = map (exacta x) ys

exacta :: MonName -> ChName -> (MonName, ChName)
exacta x y = (x, y)

show0 :: Int -> String -> String
show0 n x 
  | num >= 0  = replicate num '0' ++ x
  | otherwise = x
  where num = n - (length x)

(++++) :: String -> String -> String
(++++) xs ys = xs ++ " " ++ ys
