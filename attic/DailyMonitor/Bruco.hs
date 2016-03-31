
import Control.Monad (zipWithM, forM, liftM)
import Data.List (sort)
import Data.Maybe (fromJust)
import Data.Packed.Matrix (fromColumns, toLists)
import System.Environment (getArgs)

import HasKAL.DataBaseUtils.FrameFull.Function (kagraWaveDataGetC)
import HasKAL.MonitorUtils.CoherenceMon.Function (coherenceMonW')
import HasKAL.PlotUtils.HROOT.PlotGraph 
import HasKAL.TimeUtils.GPSfunction (time2gps)
import HasKAL.WebUtils.DailySummaryPage (startHTML, startBODY, addTitle, addStyle, addDate, endHTML)
import HasKAL.WebUtils.Javascript.Function (expandFont)
import HasKAL.WaveUtils.Data (WaveData(..))
import HasKAL.WaveUtils.Function (getMaximumChunck, getCoincidentData)


main = do
  args <- getArgs
  (year, month, day, chlst) <- case length args of
                             4 -> return (args!!0, show0 2 (args!!1), show0 2 (args!!2), args!!3)
                             _ -> error "Usage: Bruco yyyy mm dd ch.lst"

  {-- parameters --}
  let gps = read $ time2gps $ year+-+month+-+day++" 00:00:00 JST"
      duration = 86400 -- seconds
      -- for Bruco
      ch1 = "K1:LSC-MICH_ERR_CAL_OUT_DQ" -- <- 後で変える
      fftLength = 1 -- seconds
      -- for Result
      oFile = year+-+month+-+day++"_Bruco.html"

  {-- read data --}
  mbWd1 <- kagraWaveDataGetC (fromIntegral gps) (fromIntegral duration) ch1
  let wss1 = case mbWd1 of
             Just x  -> x
             Nothing -> error $ "Can't find data: "++ch1++"-"++year++"/"++month++"/"++day

  chs <- liftM ((filter (/=ch1)).lines) $ readFile chlst
  mbWd2 <- mapM (kagraWaveDataGetC (fromIntegral gps) (fromIntegral duration)) chs
  let (wss2, ch2) = unzip . filter (\(x,_) -> x/=Nothing) $ zip mbWd2 chs
      (ws1:ws2) = getCoincidentData (wss1 : map fromJust wss2)

  {-- main --}
  case ws1 /= [] of
   True -> do
     let (wd1:wd2) = map getMaximumChunck (ws1:ws2)
     result <- hBrucoPng (year+-+month+-+day) fftLength (wd1, ch1) $ zip wd2 ch2
     let body = geneRankTable (year,month,day) ch1 result
     writeFile oFile $
       startHTML
       ++ addStyle
       ++ startBODY
       ++ addTitle "Bruco"
       ++ addDate (year+-+month+-+day)
       ++ body
       ++ endHTML
   False -> do
     error $ concat $ ("Channels are not active at the same time.\n   "++ch1) : map ("\n   "++) ch2

  

{-- Internal Functions --}
hBrucoPng :: String -> Double -> (WaveData, String) -> [(WaveData , String)] -> IO [(Double, [(Double, String)])]
hBrucoPng date sec (wd1, ch1) wd2 = do
  let cohResults = coherenceMonW' sec wd1 (map fst wd2)
  zipWithM (\x y -> plotV Linear Line 1 BLUE ("frequency [Hz] at "++date, "|coh(f)|^2") 0.05 (ch1++" vs "++x)
                    (ch1+-+x+-+date++"_Bruco.png") ((0,0),(0,0)) y) (map snd wd2) cohResults
  let cohList = toLists.fromColumns $ map snd cohResults
      fvec = [0, 1/sec..]
      result = map (ranked.labeled) cohList
        where ranked x = reverse $ sort x
              labeled x = zip x (map snd wd2)
  return $ zip fvec result

(+-+) x y = x ++ "-" ++ y


geneRankTable :: (String,String,String) -> String -> [(Double, [(Double, String)])] -> String 
geneRankTable (yyyy,mm,dd) channel1 xs = concat [
  "<h3>Channel: "++channel1++"</h3>",
  expandFont 3 1 "resizable",
  "<table cellspacing=\"10\"><tr>",
  concat $ map (\n -> "<th><nobr>"++(show.fst.head $ drop (len*n) xs)++"Hz~</nobr></th>") [0..(numRow-1)],
  "</tr><tr>",
  concat $ map (\n -> concat [
                   "<td><table class=\"resizable\" border=\"0\" cellpadding=\"0\" cellspacing=\"0\" style=\"font-size:3px;\">",
                   "<tr bgcolor=\"cccccc\"><th>freq. [Hz]</th>",
                   concat $ map nthLabel [1..numNth],
                   concat $ map (geneRankTableCore numNth (yyyy,mm,dd) channel1) $ take len $ drop (len*n) xs,
                   "</table></td>"]) [0..(numRow-1)],
  "</tr></table>"]
  where len = length xs `div` numRow
        numRow = 5
        numNth = 5
        nthLabel 1 = "<th>1st ch.</th>"
        nthLabel 2 =  "<th>2nd ch.</th>"
        nthLabel 3 =  "<th>3rd ch.</th>"
        nthLabel n = "<th>"++(show n)++"th ch.</th>"

geneRankTableCore :: Int -> (String,String,String) -> String -> (Double, [(Double, String)]) -> String
geneRankTableCore n (yyyy,mm,dd) ch1 (freq, res) = concat [
  "<tr><th bgcolor=\"#cccccc\"><nobr>"++(show freq)++" Hz&emsp;</nobr></th>",
  concat.(take n') $ map (\(val, ch) -> "<td bgcolor="++(color val)++"><nobr>"
                                        ++"<a href=\""++url ch++"\" >"
                                        ++ch
                                        ++"</a>"
                                        ++"&emsp;</nobr><br>") res,
  "</tr>"]
  where color val | val > 0.8 = "\"#ff5555\""
                  | val > 0.6 = "\"#ffaaaa\""
                  | val > 0.4 = "\"#ffeeee\""
                  | otherwise = "\"#ffffff\""
        n' = min n (length res)
        url ch2 = "./"++ch1+-+ch2+-+yyyy+-+mm+-+dd++"_Bruco.png"


show0 :: Int -> String -> String
show0 digit number
  | len < digit = (concat $ replicate (digit-len) "0") ++ number
  | otherwise   = number
  where len = length number

