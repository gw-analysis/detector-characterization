{- |
Module      : multiChannelViewer.cgi
Description : This is documentation tests.
Copyright   : (c) WhoAmI, 2014
License     : ???
Maintainer  : Takahiro Yamamoto %mail%
Stability   : test
Portability : POSIX

-}{-
  * Last Modified: 2015/07/21 22:38:47
-}

import Network.CGI
import System.Directory (doesFileExist)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (forM_, liftM)
import Data.Maybe (fromJust)
import Data.Vector.Storable as S(toList, maximum)
import Data.List (isInfixOf)
import Numeric (showFFloat)

import HasKAL.Misc.StrictMapping
import HasKAL.DataBaseUtils.Function
import HasKAL.FrameUtils.FrameUtils
import HasKAL.TimeUtils.GPSfunction
import HasKAL.MonitorUtils.CorrelationMon.CalCorrelation
import HasKAL.MonitorUtils.CorrelationMon.CorrelationMethod
import HasKAL.MonitorUtils.CoherenceMon.Function
import CommonForm


pngpath :: String
pngpath = "../mon_images/"

xendCh :: [String]
xendCh = ["K1:PEM-EX_ACC_NO2_X_FLOOR"
         ,"K1:PEM-EX_ACC_NO2_Y_FLOOR"
         ,"K1:PEM-EX_ACC_NO2_Z_FLOOR"
         ,"K1:PEM-EX_MAG_X_FLOOR"
         ,"K1:PEM-EX_MAG_Y_FLOOR"
         ,"K1:PEM-EX_MAG_Z_FLOOR"
         ,"K1:PEM-EX_MIC_FLOOR"
         ]

inputForm :: String -> String -> String
inputForm gps script = concat [
  "<form action=\"", script, "\" method=\"GET\">",
  dateForm gps,
  "<p>Duration: <input type=\"text\" name=\"duration\" value=\"32\" size=\"5\" /> sec.</p>",
  channelForm Single xendCh,
  "<br>(only Bruco)",
  "<h3>Method: </h3>",
  "<input type=\"radio\" name=\"method\" value=\"Bruco\" checked=\"checked\"> Bruco</p>",
  "<input type=\"radio\" name=\"method\" value=\"Peason\"> Peason</p>",
  -- "<input type=\"radio\" name=\"method\" value=\"MIC\"> MIC</p>",
  "<input type=\"submit\" value=\"view\" />",
  "</form>"]


printBruco :: (Double -> String -> String) -> Int -> (Double, [(Double, String)]) -> String
printBruco url n (freq, res) = concat [
  "<tr><th bgcolor=\"#cccccc\"><nobr>"++(show freq)++" Hz&emsp;</nobr></th>",
  concat.(take n') $ map (\(val, ch) -> "<td bgcolor="++(color val)++"><nobr><a href=\""++url freq ch
                                        ++"\" target=\"_blank\">"++ch++"</a>&emsp;</nobr><br>") res,
  "</tr>"]
  where color val | val > 0.8 = "\"#ff5555\""
                  | val > 0.6 = "\"#ffaaaa\""
                  | val > 0.4 = "\"#ffeeee\""
                  | otherwise = "\"#ffffff\""
        n' = min n (length res)

brucoWeb :: String -> String -> String -> [(Double, [(Double, String)])] -> String
brucoWeb gps duration channel1 coh'result = concat [
  "<div>gps time: " ++ gps,
  "<br>duration: " ++ duration,
  "<br> channel: " ++ channel1 ++ "</div><br>",
  "<table cellspacing=\"10\"><tr>",---
  concat $ map (\n -> "<th><nobr>"++(show.fst.head $ drop (len*n) coh'result)++"Hz~</nobr></th>") [0..(n-1)],
  "</tr><tr>",
  concat $ map (\n -> concat [
                   "<td><table border=\"0\" cellpadding=\"0\" cellspacing=\"0\" style=\"font-size:3px;\">",
                   "<tr bgcolor=\"cccccc\"><th>freq. [Hz]</th><th>1st ch.</th>",
                   "<th>2nd ch.</th><th>3rd ch.</th><th>4th ch.</th><th>5th ch.</th></tr>",
                   concat $ map (printBruco url 5) $ take len $ drop (len*n) coh'result,
                   "</table></td>"]) [0..(n-1)],
  "</tr></table>",
  timeShiftLink "./multiChannelViewer2.cgi" gps duration uris
  ]
  where uris = "&channel=" ++ channel1 ++ "&method=Bruco"
        len = length coh'result `div` n
        n = 5
        url freq channel2 = "./brucoDetail.cgi?gps="++gps++"&duration="++duration++"&channel1="
                            ++channel1++"&channel2="++channel2++"&freq="++(show freq)

printPeason :: String -> [Double] -> String
printPeason ch results = concat [
  "<tr><th bgcolor=\"#eeeeee\">"++ch++" </th>",
  concat $ map (\x -> "<td bgcolor="++(color x)++">"++(showFFloat (Just 5) x "")++"</td>") results,
  "</tr>"]
  where color val | val > 0.8    = "\"#ff5555\""
                  | val > 0.5    = "\"#ffaaaa\""
                  | val > 0.2    = "\"#ffeeee\""
                  | val > (-0.2) = "\"#ffffff\""
                  | val > (-0.5) = "\"#eeeeff\""
                  | val > (-0.8) = "\"#aaaaff\""
                  | otherwise    = "\"#5555ff\""

peasonWeb :: String -> String -> ([String], [[Double]]) -> String
peasonWeb gps duration (ch, result) = concat [
  "<div>gps time: " ++ gps,
  "<br>duration: " ++ duration ++ "</div><br>",
  "<table border=\"1\" cellpadding=\"6\">",
  "<tr bgcolor=\"dddddd\"><th>freq. [Hz]</th>",
  concat $ map (\x -> "<th>"++x++"</th>") $ ch,
  "</tr>",
  concat $ zipWith printPeason ch result,
  "</table>",
  timeShiftLink "./multiChannelViewer2.cgi" gps duration uris
  ]
  where uris = "&channel=" ++ (ch!!0)

corMain :: String -> String -> String -> String -> IO String
corMain gps duration methods ch1 = do
  case methods of
   "Bruco" -> do
     fileMaybe <- kagraDataFind (read gps) (read duration) ch1
     case fileMaybe of
      Nothing -> return $ "<h4 style=\"color:#ff0000;\">&emsp;Can't find file or channel-1: "++ch1++"</h4>"
                 ++(timeShiftLink "./multiChannelViewer2.cgi" gps duration $ "&channel="++ch1++"&method="++methods)
      _       -> do
        datMaybe1 <- kagraDataGet (read gps) (read duration) ch1
        chlist <- liftM fromJust $ getChannelList $ (fromJust fileMaybe)!!0
        let chlist' = filter (/=ch1) $ filter (isInfixOf "K1:") $ map fst chlist
        datMaybe2 <- mapM (kagraDataGet (read gps) (read duration)) $ filter (/=ch1) chlist'
        let result = hBruco 2048 2048 (fromJust datMaybe1, ch1) $ zip (map fromJust datMaybe2) chlist'
        return $ brucoWeb gps duration ch1 result
   "Peason" -> do
     chk <- kagraDataGet (read gps) (read duration) (ch1)
     case chk of
      Nothing -> return $ "Can't find file or channel-1: "++(ch1)
      _       -> do
        result <- forM' xendCh $ \ch1 -> do
          datMaybe1 <- kagraDataGet (read gps) (read duration) ch1
          forM' xendCh $ \ch2 -> do
            datMaybe2 <- kagraDataGet (read gps) (read duration) ch2
            return $ S.maximum $ takeCorrelationV (read methods) (fromJust datMaybe1) (fromJust datMaybe2) 10
        return $ peasonWeb gps duration (xendCh, result)

body :: Maybe String -> Maybe String -> Maybe String -> Maybe String -> String -> String
body gps duration methods channel1 script =
  unsafePerformIO $ case (gps, duration, methods ,channel1) of
                     (Just "", _, _, _) -> do
                       nowGps <- return "1120543424" -- getCurrentGps
                       return $ inputForm nowGps script
                     (Just x, Just "", _, _) -> return $ inputForm x script
                     (Just x, _, Just "", _) -> return $ inputForm x script
                     (Just x, _, _, Nothing) -> return $ inputForm x script
                     (Just x, Just y, Just z, Just w) -> corMain x y z w
                     (_, _, _, _) -> do
                       nowGps <- return "1120543424" -- getCurrentGps
                       return $ inputForm nowGps script

cgiMain :: CGI CGIResult
cgiMain = do
  setHeader "Content-type" "text/html; charset = UTF-8"
  script <- scriptName
  date <- getInput "Date"
  gps <- case date of
    Just "GPS" -> getInput "gps"
    Just "Local" -> do
      year <- getInput "year"
      month <- getInput "month"
      day <- getInput "day"
      hour <- getInput "hour"
      minute <- getInput "minute"
      second <- getInput "second"
      local <- getInput "local"
      return $ Just $ time2gps $ (fromJust year)++"-"++(fromJust month)++"-"++(fromJust day)++" "
        ++(fromJust hour)++":"++(fromJust minute)++":"++(fromJust second)++" "++(fromJust local)
    _ -> return $ Just ""
  duration <- getInput "duration"
  channel1 <- getInput "channel"
  methods <- getInput "method"
  output $ hkalFrame "Multi Channel Viewer (new ver.)" $ body gps duration methods channel1 script

main :: IO ()
main = runCGI (handleErrors cgiMain)
