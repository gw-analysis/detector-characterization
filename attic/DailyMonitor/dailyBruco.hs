
import Data.Maybe (fromJust)
import Control.Monad (forM, liftM)
import System.Environment (getArgs)
import Data.Packed.Vector (subVector, dim, fromList)

import HasKAL.TimeUtils.GPSfunction (time2gps)
import HasKAL.FrameUtils.FrameUtils (getSamplingFrequency)
import HasKAL.DataBaseUtils.Function (kagraDataGet, kagraDataFind)
import HasKAL.MonitorUtils.CoherenceMon.Function

main = do
  args <- getArgs
  (year, month, day, chlst) <- case length args of
                             4 -> return (args!!0, show0 2 (args!!1), show0 2 (args!!2), args!!3)
                             _ -> error "Usage: dailyBruco yyyy mm dd ch.lst"

  {-- parameters --}
  let gps = read $ time2gps $ year++"-"++month++"-"++day++" 00:00:00 JST"
      duration = 1024 --86400 -- seconds
      -- for Bruco
      gwCh = "K1:PEM-EX_REF" -- <- 後で変える
      fftLength = 1 -- seconds
      -- for Result
      oFile = gwCh++"-"++year++"-"++month++"-"++day++"_dailyBruco.html"

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
  let body = geneRankTable gwCh $ hBruco fftLength (fs1, dat1, gwCh) $ filter ((/=0).(\(x,_,_) -> x)) dat2
  writeFile oFile $ body


{-- Internal Functions --}
geneRankTable :: String -> [(Double, [(Double, String)])] -> String 
geneRankTable channel1 xs = concat [
  "<h3>Channel: "++channel1++"<h3>",
  "<table cellspacing=\"10\"><tr>",
  concat $ map (\n -> "<th><nobr>"++(show.fst.head $ drop (len*n) xs)++"Hz~</nobr></th>") [0..(numRow-1)],
  "</tr><tr>",
  concat $ map (\n -> concat [
                   "<td><table border=\"0\" cellpadding=\"0\" cellspacing=\"0\" style=\"font-size:3px;\">",
                   "<tr bgcolor=\"cccccc\"><th>freq. [Hz]</th>",
                   concat $ map nthLabel [1..numNth],
                   concat $ map (geneRankTableCore numNth) $ take len $ drop (len*n) xs,
                   "</table></td>"]) [0..(numRow-1)],
  "</tr></table>"]
  where len = length xs `div` numRow
        numRow = 3
        numNth = 5
        nthLabel 1 = "<th>1st ch.</th>"
        nthLabel 2 =  "<th>2nd ch.</th>"
        nthLabel 3 =  "<th>3rd ch.</th>"
        nthLabel n = "<th>"++(show n)++"th ch.</th>"

geneRankTableCore :: Int -> (Double, [(Double, String)]) -> String
geneRankTableCore n (freq, res) = concat [
  "<tr><th bgcolor=\"#cccccc\"><nobr>"++(show freq)++" Hz&emsp;</nobr></th>",
  concat.(take n') $ map (\(val, ch) -> "<td bgcolor="++(color val)++"><nobr>"
                                        -- ++"<a href=\""++url freq ch++"\" target=\"_blank\">"
                                        ++ch
                                        -- ++"</a>"
                                        ++"&emsp;</nobr><br>") res,
  "</tr>"]
  where color val | val > 0.8 = "\"#ff5555\""
                  | val > 0.6 = "\"#ffaaaa\""
                  | val > 0.4 = "\"#ffeeee\""
                  | otherwise = "\"#ffffff\""
        n' = min n (length res)
        -- url freq ch2 = "./main2.cgi?Date=GPS&gps="++(fromJust $ gps params)++"&duration="++(duration params)++"&channel1="
        --                ++(head $ channel1 params)++"&channel2="++ch2++"&monitor="++(head $ monitors params)
        --                ++"&fmin="++(show $freq-10)++"&fmax="++(show $freq+10)


show0 :: Int -> String -> String
show0 digit number
  | len < digit = (concat $ replicate (digit-len) "0") ++ number
  | otherwise   = number
  where len = length number

