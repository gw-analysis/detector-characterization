{- |
Module      : pastDataViewer.cgi
Description : This is documentation tests.
Copyright   : (c) WhoAmI, 2014
License     : ???
Maintainer  : Takahiro Yamamoto %mail%
Stability   : test
Portability : POSIX

-}{-
  * Last Modified: 2015/07/16 12:03:21
-}

import Network.CGI
import Control.Monad (forM_, liftM)
import System.Directory (doesFileExist)
import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe (fromJust)
import qualified Data.Vector.Storable as V (fromList, length)

import HasKAL.SpectrumUtils.SpectrumUtils
import HasKAL.PlotUtils.HROOT.PlotGraph
import HasKAL.PlotUtils.HROOT.PlotGraph3D
import HasKAL.SpectrumUtils.Function
import HasKAL.DataBaseUtils.Function
import HasKAL.TimeUtils.GPSfunction
import HasKAL.FrameUtils.FrameUtils (getSamplingFrequency)
import CommonForm

pngpath :: String
pngpath = "../env_images/"

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
  channelForm Multi xendCh,
  "<h3> Type: </h3>",
  "<p><input type=\"checkbox\" name=\"plottype\" value=\"TS\" checked=\"checked\">Time Series</p>",
  "<p><input type=\"checkbox\" name=\"plottype\" value=\"PSD\" checked=\"checked\">Spectrum</p>",
  "<p><input type=\"checkbox\" name=\"plottype\" value=\"SPE\" checked=\"checked\">Spectrogram</p>",
  "<input type=\"submit\" value=\"plot view\" />",
  "</form>"]

putName :: String -> String -> [String] -> String -> String -> String
putName gps duration plottypes channel msg = concat [
  "<Hr><h3> Channel: ", channel, "</h3>",
  case msg of "" -> (func plottypes)++"<br><br><br>"
              _ -> "<h4 style=\"color:#ff0000;\">&emsp;"++msg++"</h4>"
  ]
  where func ss = concat $ map (\s -> concat ["<nobr><a href=\"", pngpath, channel, "_", s, "-", gps, "-", duration, ".png\">",
                                              "<img alt=\"\" src=\"", pngpath, channel, "_", s, "-", gps, "-", duration, ".png\"",
                                              "style=\"border: 0px solid ; width: 300px;\"></a>", "</nobr>"]) ss
-- where func' ss = concat [
  --         "<table><tr>",
  --         concat $ map (\s -> concat ["<td><a href=\"", pngpath, channel, "_", s, "-", gps, "-", duration, ".png\">",
  --                                   "<img alt=\"\" src=\"", pngpath, channel, "_", s, "-", gps, "-", duration, ".png\"",
  --                                   "style=\"border: 0px solid ; width: 300px;\"></a></td>"]) ss, "</tr><tr>",
  --         concat $ map (\s -> concat ["<td><a href=\"", "../", channel, "_", s, "-", gps, "-", duration, ".txt\" download=\"",
  --                                   channel, "_", s, "-", gps, "-", duration, ".txt\"> Download </a></td>"]) ss, "</tr></table>"]

putNames :: String -> String -> [String] -> [String] -> [String] -> String
putNames gps duration plottypes channels msgs = concat [
  "<h2>GPS Time: ", gps, "&nbsp; (", (gps2localTime (read gps) "JST"), ")</h2>",
  (concat $ zipWith (putName gps duration plottypes) channels msgs),
  "<Hr>[<a href=\"./pastDataViewer.cgi?Date=GPS&gps=", (show $ (read gps) - (read duration)), uris, "\">&lt; Prev</a>] ",
  " [<a href=\"./pastDataViewer.cgi\">Back</a>] ",
  " [<a href=\"./pastDataViewer.cgi?Date=GPS&gps=", (show $ (read gps) + (read duration)), uris, "\">Next &gt;</a>]"
  ]
  where uris = "&duration=" ++ duration
               ++ (concat $ zipWith (++) (repeat "&channel=") channels)
               ++ (concat $ zipWith (++) (repeat "&plottype=") plottypes)

monMain :: String -> String -> [String] -> String -> IO String
monMain gps duration pts ch = do
  datMaybe <- kagraDataGet (read gps) (read duration) ch
  case datMaybe of
   Nothing -> return "Can't find file or channel"
   _ -> do
     let dat = fromJust datMaybe
     fs <- (`getSamplingFrequency` ch) =<< liftM (head.fromJust) (kagraDataFind (read gps) 1 ch)
     forM_ pts $ \pt -> do
       pngExist <- doesFileExist $ pngpath++ch++"_"++pt++"-"++gps++"-"++duration++".png"
       case pngExist of
        True -> return ()
        False -> do
          case pt of
           "TS" -> do
             let tvec = V.fromList $ take (V.length dat) [0,1/2048..]
             plotV Linear Line 1 BLUE ("s", "") 0.05 pt (pngpath++ch++"_"++pt++"-"++gps++"-"++duration++".png") ((0,0),(0,0)) (tvec, dat)
           "PSD" -> do
             let hfs = gwpsdV dat (truncate fs) fs
             plotV LogXY Line 1 BLUE ("Hz", "/rHz") 0.05 pt (pngpath++ch++"_"++pt++"-"++gps++"-"++duration++".png") ((0,0),(0,0)) hfs
           "SPE" -> do
             let hfs = gwspectrogramV 0 (truncate fs) fs dat
             spectrogramM LogZ COLZ "/rHz" pt (pngpath++ch++"_"++pt++"-"++gps++"-"++duration++".png") ((0,0), (0,0)) hfs
     return ""
     
body :: Maybe String -> Maybe String -> [String] -> [String] -> String -> String
body gps duration plottypes channels script =
  unsafePerformIO $ case (gps, duration, plottypes, channels) of
     (Just "", _, _, _) -> do
       nowGps <- getCurrentGps
       return $ inputForm nowGps script
     (Just x, Just "", _, _) -> return $ inputForm x script
     (Just x, _, [], _) -> return $ inputForm x script
     (Just x, _, _, []) -> return $ inputForm x script
     (Just x, Just y, z, w) -> do
       msgs <- mapM (monMain x y z) w
       return $ putNames x y z w msgs
     (_, _, _, _) -> do
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
  duration <- getInput "duration"
  plottypes <- getMultiInput "plottype"
  channels <- getMultiInput "channel"
  output $ hkalFrame "Past Data Viewer" $ body gps duration plottypes channels script

main :: IO ()
main = runCGI (handleErrors cgiMain)
