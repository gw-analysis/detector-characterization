{- |
Module      : webMonitor.cgi
Description : This is documentation tests.
Copyright   : (c) WhoAmI, 2014
License     : ???
Maintainer  : Takahiro Yamamoto %mail%
Stability   : test
Portability : POSIX

-}{-
  * Last Modified: 2015/07/12 17:28:27
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

inputForm :: String -> String
inputForm script = concat [
  "<form action=\"", script, "\" method=\"GET\">",
  dateForm,
  channelForm Multi xendCh,
  "<h3> Monitor: </h3>",
  "<p><input type=\"checkbox\" name=\"monitor\" value=\"RM\" checked=\"checked\">RayleighMon</p>",
  "<p><input type=\"checkbox\" name=\"monitor\" value=\"SRM\" >StudentRayleighMon</p>",
  "<input type=\"submit\" value=\"view\" />",
  "</form>"]

putName :: String -> [String] -> String -> String
putName gps monitors channel = concat [
  "<Hr><h3> Channel: ", channel, "</h3>",
  (concat $ map func monitors),
  "<br><br><br>"]
  where func s = concat [
          "<nobr><a href=\"", pngpath, channel, "_", s, "-", gps, "-", "128", ".png\">",
          "<img alt=\"\" src=\"", pngpath, channel, "_", s, "-", gps, "-", "128", ".png\"",
          "style=\"border: 0px solid ; width: 300px;\"></a>", "</nobr>"]

putNames :: String -> [String] -> [String] -> String
putNames gps monitors channels = concat [
  "<h2>GPS Time: ", gps, "</h2>",
  (concat $ map (putName gps monitors) channels),
  "<Hr>[<a href=\"./webMonitor.cgi?gps=", (show $ (read gps) - 32), uris, "\">&lt; Prev</a>] ",
  " [<a href=\"./webMonitor.cgi\">Back</a>] ",
  " [<a href=\"./webMonitor.cgi?Date=GPS&gps=", (show $ (read gps) + 32), uris, "\">Next &gt;</a>]"
  ]
  where uris = (concat $ zipWith (++) (repeat "&channel=") channels)
               ++ (concat $ zipWith (++) (repeat "&monitor=") monitors)
                 
monMain :: String -> [String] -> String -> IO ()
monMain gps mons ch = do
  datMaybe <- kagraDataGet (read gps) (read "128") ch
  case datMaybe of
   Nothing -> return ()
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

body :: Maybe String -> [String] -> [String] -> String -> String
body gps monitors channels script =
    unsafePerformIO $ case (gps, monitors, channels) of
                       (Just "", _, _) -> return $ inputForm script
                       (_, [], _) -> return $ inputForm script
                       (_, _, []) -> return $ inputForm script
                       (Just x, y, z)  -> do
                         mapM_ (monMain x y) z
                         return $ putNames x y z
                       (_, _, _) -> return $ inputForm script

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

