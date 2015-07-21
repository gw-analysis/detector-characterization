{- |
Module      : webMonitor.cgi
Description : This is documentation tests.
Copyright   : (c) WhoAmI, 2014
License     : ???
Maintainer  : Takahiro Yamamoto %mail%
Stability   : test
Portability : POSIX

-}{-
  * Last Modified: 2015/07/15 18:17:23
-}

import Network.CGI
import Control.Monad (forM_, liftM)
import Data.Maybe (fromJust)
import System.Directory (doesFileExist)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Vector.Storable as V (concat)

import HasKAL.SpectrumUtils.SpectrumUtils
import HasKAL.FrameUtils.FrameUtils
import HasKAL.PlotUtils.HROOT.PlotGraph
import HasKAL.SpectrumUtils.Function
import HasKAL.TimeUtils.GPSfunction
import HasKAL.DataBaseUtils.Function
import HasKAL.MonitorUtils.SRMon.StudentRayleighMon
import HasKAL.MonitorUtils.RayleighMon.RayleighMon
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
  channelForm Multi xendCh,
  "<h3> Monitor: </h3>",
  "<p><input type=\"checkbox\" name=\"monitor\" value=\"RM\" checked=\"checked\">RayleighMon</p>",
  "<p><input type=\"checkbox\" name=\"monitor\" value=\"SRM\" >StudentRayleighMon</p>",
  "<input type=\"submit\" value=\"view\" />",
  "</form>"]

putName :: String -> [String] -> String -> String -> String
putName gps monitors channel msg = concat [
  "<Hr><h3> Channel: ", channel, "</h3>",
  case msg of "" -> (func monitors)++"<br><br><br>"
              _ -> "<h4 style=\"color:#ff0000;\">&emsp;"++msg++"</h4>"
  ]
  where func ss = concat $ map (\s -> concat ["<nobr><a href=\"", pngpath, channel, "_", s, "-", gps, "-", "128", ".png\">",
                                              "<img alt=\"\" src=\"", pngpath, channel, "_", s, "-", gps, "-", "128", ".png\"",
                                              "style=\"border: 0px solid ; width: 300px;\"></a>", "</nobr>"]) ss
  -- where funcs' ss = concat [
  --         "<table><tr>",
  --         concat $ map (\s -> concat ["<td><a href=\"", pngpath, channel, "_", s, "-", gps, "-", "128", ".png\">",
  --                                     "<img alt=\"\" src=\"", pngpath, channel, "_", s, "-", gps, "-", "128", ".png\"",
  --                                     "style=\"border: 0px solid ; width: 300px;\"></a><//td>"]) ss, "</tr><tr>",
  --         concat $ map (\s -> concat ["<td><a href=\"", "../", channel, "_", s, "-", gps, "-", "128", ".txt\" download=\"",
  --                                     channel, "_", s, "-", gps, "-", "128", ".txt\"> Download </a></td>"]) ss, "</tr></table>"]

          
putNames :: String -> [String] -> [String] -> [String] -> String
putNames gps monitors channels msgs = concat [
  "<h2>GPS Time: ", gps, "&nbsp; (", (gps2localTime (read gps) "JST"), ")</h2>",
  (concat $ zipWith (putName gps monitors) channels msgs),
  timeShiftLink "./webMonitor.cgi" gps "1" uris
  ]
  where uris = (concat $ zipWith (++) (repeat "&channel=") channels)
               ++ (concat $ zipWith (++) (repeat "&monitor=") monitors)
                 
monMain :: String -> [String] -> String -> IO String
monMain gps mons ch = do
  datMaybe <- kagraDataGet (read gps) (read "128") ch
  case datMaybe of
   Nothing -> return "Can't find file or channel"
   _ -> do
     let dat = fromJust datMaybe
     fs <- (`getSamplingFrequency` ch) =<< liftM (head.fromJust) (kagraDataFind (read gps) 1 ch)
     forM_ mons $ \mon -> do
       pngExist <- doesFileExist $ pngpath++ch++"_"++mon++"-"++gps++"-"++"128"++".png"
       case pngExist of
        True -> return ()
        False -> do
          case mon of
           "RM" -> do
             let snf = gwpsdV dat (truncate fs) fs
                 hfs = gwspectrogramV 0 (truncate fs) fs dat
                 qv = rayleighMonV [0.5, 0.9, 0.95, 0.99] fs (truncate fs) 16 snf hfs
             oPlotV Linear LinePoint 1 [RED, RED, BLUE, BLUE, PINK, PINK, GREEN, GREEN] ("[Hz]", "Normalized NoiseLv.") 0.05 mon
               (pngpath++ch++"_"++mon++"-"++gps++"-"++"128"++".png") ((0,0),(0,6)) (concat $ map (\(x, y) -> [x, y]) qv)
           "SRM" -> do
             let snf = gwpsdV dat (truncate fs) fs
                 hfs = gwspectrogramV 0 (truncate fs) fs dat
                 nus = studentRayleighMonV (QUANT 0.95) fs (truncate fs) 64 64 16 snf hfs
             plotV Linear LinePoint 1 BLUE ("[Hz]", "nu") 0.05 mon
               (pngpath++ch++"_"++mon++"-"++gps++"-"++"128"++".png") ((0,0),(0,0)) (getSpectrum 0 nus)
     return ""
     
body :: Maybe String -> [String] -> [String] -> String -> String
body gps monitors channels script =
  unsafePerformIO $ case (gps, monitors, channels) of
                     (Just "", _, _) -> do
                       nowGps <- getCurrentGps
                       return $ inputForm nowGps script
                     (Just x, [], _) -> return $ inputForm x script
                     (Just x, _, []) -> return $ inputForm x script
                     (Just x, y, z)  -> do
                       msgs <- mapM (monMain x y) z
                       return $ putNames x y z msgs
                     (_, _, _) -> do
                       nowGps <- getCurrentGps
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
  monitors <- getMultiInput "monitor"  
  channels <- getMultiInput "channel"
  output $ hkalFrame "Web Monitor" $ body gps monitors channels script

main :: IO ()
main = runCGI (handleErrors cgiMain)

