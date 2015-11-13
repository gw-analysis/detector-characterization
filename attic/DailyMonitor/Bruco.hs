
import Data.Maybe (fromJust)
import Control.Monad (forM, liftM)
import System.Environment (getArgs)
import Data.Packed.Vector (subVector, dim, fromList)

import HasKAL.TimeUtils.GPSfunction (time2gps)
import HasKAL.FrameUtils.FrameUtils (getSamplingFrequency)
import HasKAL.DataBaseUtils.Function (kagraDataGet, kagraDataFind)
import HasKAL.MonitorUtils.CoherenceMon.Function
import HasKAL.WebUtils.DailySummaryPage
import HasKAL.WebUtils.Javascript.Function (expandFont)
import HasKAL.PlotUtils.HROOT.PlotGraph 

import Data.Packed.Vector (Vector)
import Data.Packed.Matrix (Matrix, fromColumns, toLists)
import Control.Monad (zipWithM_)
import Data.List (sort)
import HasKAL.SpectrumUtils.Signature (Spectrum)

main = do
  args <- getArgs
  (year, month, day, chlst) <- case length args of
                             4 -> return (args!!0, show0 2 (args!!1), show0 2 (args!!2), args!!3)
                             _ -> error "Usage: dailyBruco yyyy mm dd ch.lst"

  {-- parameters --}
  let gps = read $ time2gps $ year+-+month+-+day++" 00:00:00 JST"
      duration = 86400 -- seconds
      -- for Bruco
      gwCh = "K1:PEM-EX_REF" -- <- 後で変える
      fftLength = 1 -- seconds
      -- for Result
      oFile = year+-+month+-+day++"_Bruco.html"

  {-- read data --}
  mbFiles1 <- kagraDataFind (fromIntegral gps) (fromIntegral duration) gwCh
  let file1 = case mbFiles1 of
               Nothing -> error $ "Can't find file: "++year++"/"++month++"/"++day
               _ -> head $ fromJust mbFiles1
  mbDat1 <- kagraDataGet gps duration gwCh
  mbFs1 <- getSamplingFrequency file1 gwCh
  let (dat1, fs1) = case (mbDat1, mbFs1) of
                     (Just a, Just b) -> (a, b)
                     (Nothing, _) -> error $ "Can't read data: "++gwCh++"-"++year++"/"++month++"/"++day
                     (_, Nothing) -> error $ "Can't read sampling frequency: "++gwCh++"-"++year++"/"++month++"/"++day

  chs <- liftM ((filter (/=gwCh)).lines) $ readFile chlst
  dat2 <- forM chs $ \ch -> do
    mbDat2 <- kagraDataGet gps duration ch
    mbFs2 <- getSamplingFrequency file1 ch
    case (mbFs2, mbDat2) of
     (Just a, Just b) -> return (a, b, ch)
     (Nothing, _) -> return (0, fromList [], "")
     (_, Nothing) -> return (0, fromList [], "")

  {-- main --}
  result <- hBrucoPng (year+-+month+-+day) fftLength (fs1, dat1, gwCh) $ filter ((/=0).(\(x,_,_) -> x)) dat2
  let body = geneRankTable (year,month,day) gwCh result
  writeFile oFile $
    startHTML
    ++ addStyle 
    ++ startBODY 
    ++ addTitle (year+-+month+-+day) "Bruco"
    ++ body
    ++ endHTML
  

{-- Internal Functions --}
hBrucoPng :: String -> Double -> (Double, Vector Double, String) -> [(Double, Vector Double, String)] -> IO [(Double, [(Double, String)])]
hBrucoPng dateStr sec (fsx, xt, xch) yts = do
  let cohResults = map (\x -> coherenceMon sec fsx (fst' x) xt (snd' x) ) yts
  zipWithM_ (\x y -> plotV Linear Line 1 BLUE ("frequency [Hz] at "++dateStr, "coh(f)^2") 0.05 (xch++" vs "++x) (xch+-+x+-+dateStr++"_dailyBruco.png") ((0,0),(0,0)) y)
    (map trd' yts) cohResults
  let cohList = toLists.fromColumns $ map snd cohResults
      fvec = [0, 1/sec..]
      result = map (ranked.labeled) cohList
        where ranked x = reverse $ sort x
              labeled x = zip x (map trd' yts)
  return $ zip fvec result

fst' (a,_,_) = a
snd' (_,b,_) = b
trd' (_,_,c) = c


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
        url ch2 = "./"++ch1+-+ch2+-+yyyy+-+mm+-+dd++"_dailyBruco.png"


show0 :: Int -> String -> String
show0 digit number
  | len < digit = (concat $ replicate (digit-len) "0") ++ number
  | otherwise   = number
  where len = length number

