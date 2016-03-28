
import System.Environment (getArgs)
import Control.Monad (liftM, forM)
import Data.List ((\\), nub, foldl', delete, isPrefixOf,sort,elemIndices)

type ChName = String
type MonName = String

data Monitor = TimeSeries | Spectrum | Spectrogram
             | RMSMon     | SensMon  | LTF
             | RMon       | SRMon    | LTA
             | RangeMonNSNS
  deriving ( Eq, Ord, Show, Read)


main = do
  (chlist, monlist) <- getArgs >>= \args -> case (length args) of
    2 -> return (args!!0,args!!1)
    _ -> error "Usage: test chlist monlist"

  chs <- readFile chlist >>= \x -> return $ removeCommentLines (lines x)
  mons <- readFile monlist >>= \x -> return $ removeCommentLines (lines x)
  let mons' = map show $ sort $ map (\x->read x :: Monitor) (dealingwithLT mons)

  print $ filterRule [(x,y)| y<-chs, x<-mons']





removeCommentLines = concatMap (\x-> words $ take (head' x $ elemIndices '#' x) x)

head' y x | x ==[] = length y | otherwise = head x

dealingwithLT xs = ["LTF"|x<-xs,x=="LT"] ++ ["LTA"|x<-xs,x=="LT"] ++[x|x<-xs,x/="LT"]





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
    rule11= limitMonitors "K1:LSC-MICH_ERR_CAL_OUT_DQ" ["SensMon","RMSMon","RMon","SRMon","Spectrum","Spectrogram","LT","RangeMonNSNS"]


limitMonitors :: ChName -> [MonName] -> [(MonName, ChName)] -> [(MonName, ChName)]
limitMonitors ch mons orig = multiDelete del orig
  where del = multiDelete (map (flip exacta ch) mons) $ filter ((==ch).snd) orig


exacta :: MonName -> ChName -> (MonName, ChName)
exacta x y = (x, y)


multiDelete :: [(MonName, ChName)] -> [(MonName, ChName)] -> [(MonName, ChName)]
multiDelete del orig = foldl' (\bs a -> delete a bs) orig del


