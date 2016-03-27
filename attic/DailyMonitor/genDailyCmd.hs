

import System.Environment (getArgs)
import Control.Monad (liftM, forM)
import Data.List ((\\), nub, foldl', delete, isPrefixOf)

type ChName = String
type MonName = String

------------------------------------------------------------------------------
--  Filter rule
------------------------------------------------------------------------------
filterRule :: [(MonName, ChName)] -> [(MonName, ChName)]
filterRule = myfilter.nub
  where 
    -- enabled filters
    myfilter = rule1 .
               rule2 .
               rule3 .
               rule4 .
               rule5 .
               rule6 .
               rule7 .
               rule8 . 
               rule9 .
               rule10 .
               rule11

    -- each filters
    rule1 = limitMonitors "K1:GRD-PSL_STATE_N" ["TimeSeries"]
    rule2 = limitMonitors "K1:GRD-IMC_LOCK_STATE_N" ["TimeSeries"]
    rule3 = limitMonitors "K1:GRD-IFO_STATE_N" ["TimeSeries"]
    rule4 = limitMonitors "K1:GRD-LSC_LOCK_STATE_N" ["TimeSeries"]
    rule5 = limitMonitors "K1:GRD-VIS_BS_STATE_N" ["TimeSeries"]
    rule6 = limitMonitors "K1:GRD-VIS_MCE_STATE_N" ["TimeSeries"]
    rule7 = limitMonitors "K1:GRD-VIS_MCI_STATE_N" ["TimeSeries"]
    rule8 = limitMonitors "K1:GRD-VIS_MCO_STATE_N" ["TimeSeries"]
    rule9 = limitMonitors "K1:GRD-VIS_PR3_STATE_N" ["TimeSeries"]
    rule10= limitMonitors "K1:GRD-MICH_LOCK_STATE_N" ["TimeSeries"]
    rule11= limitMonitors "K1:LSC-MICH_ERR_CAL_OUT_DQ" ["SensMon","RMSMon","RMon","SRMon","Spectrum","Spectrogram","LT","RangeMonNSNS","RangeMonBHBH"]


{-- Sample Filter 
    example1 = limitMonitors "K1:Hoge" ["dailyHogeMon"]
                   -- When channel is K1:Hoge, only dailyHogeMon is applied.
                   -- When channel is another one, whole of monitors are applied.

    example2 = limitChannels "dailyFugaMon" ["K1:Fuga"]
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
  flist <- liftM (commentFilter 2.lines) $ readFile masterFile

  {-- read each file --}
  chmonlst <- liftM concat $ forM flist $ \[chfile, monfile] -> do
    monlst <- liftM (concat.(commentFilter 1).lines) $ readFile monfile :: IO [MonName]
    chlst <- liftM (concat.(commentFilter 1).lines) $ readFile chfile :: IO [ChName]
    return $ exactaBox monlst chlst :: IO [(MonName, ChName)]

  {--  filter  --}
  let chmonlst' = filterRule chmonlst

  {-- chekck  --}
  forM chmonlst' $ \(mon, ch) -> do
    putStrLn $ mon ++++ yyyy ++++ mm ++++ dd ++++ ch

{-- internal functions --}
commentFilter :: Int -> [String] -> [[String]]
commentFilter n xs = filter ((==n) . length) $ map (words . (takeWhile (/='#'))) xs

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
