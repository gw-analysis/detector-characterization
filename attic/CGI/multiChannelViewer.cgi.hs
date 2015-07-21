{- |
Module      : multiChannelViewer.cgi
Description : This is documentation tests.
Copyright   : (c) WhoAmI, 2014
License     : ???
Maintainer  : Takahiro Yamamoto %mail%
Stability   : test
Portability : POSIX

-}{-
  * Last Modified: 2015/07/21 22:38:34
-}

import Network.CGI
import System.Directory (doesFileExist)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (forM_, liftM)
import Data.Maybe (fromJust)
import Data.Vector.Storable (fromList)

import HasKAL.DataBaseUtils.Function
import HasKAL.FrameUtils.FrameUtils
import HasKAL.TimeUtils.GPSfunction
import HasKAL.MonitorUtils.CorrelationMon.CalCorrelation
import HasKAL.MonitorUtils.CorrelationMon.CorrelationMethod
import HasKAL.MonitorUtils.CoherenceMon.Function
import HasKAL.PlotUtils.HROOT.PlotGraph
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
  channelForm Both xendCh,
  "<h3>Method: </h3>",
  "<input type=\"checkbox\" name=\"method\" value=\"Coh\" checked=\"checked\"> Coherence</p>",
  "<input type=\"checkbox\" name=\"method\" value=\"Peason\"> Peason</p>",
  "<input type=\"checkbox\" name=\"method\" value=\"MIC\"> MIC</p>",
  "<input type=\"submit\" value=\"view\" />",
  "</form>"]

putName :: String -> [String] -> String -> String -> String -> String
putName gps methods channel1 channel2 msg = concat [
  "<Hr><h3> Channel: ", channel2, "</h3>",
  case msg of "" -> (func methods)++"<br><br><br>"
              _ -> "<h4 style=\"color:#ff0000;\">&emsp;"++msg++"</h4>"
  ]
  where func ss = concat $ map (\s -> concat ["<nobr><a href=\"", pngpath, channel1, "--", channel2, "_", s,"-", gps, "-", "32", ".png\">",
                                              "<img alt=\"\" src=\"", pngpath, channel1, "--", channel2, "_", s, "-", gps, "-", "32", ".png\"",
                                              "style=\"border: 0px solid ; width: 300px;\"></a>", "</nobr>"]) ss
  -- where func' ss = concat [
  --         "<table><tr>",
  --         concat $ map (\s -> concat ["<td><a href=\"", pngpath, channel1, "--", channel2, "_", s, "-", gps, "-", "32", ".png\">",
  --                                   "<img alt=\"\" src=\"", pngpath, channel1, "--", channel2, "_", s, "-", gps, "-", "32", ".png\"",
  --                                   "style=\"border: 0px solid ; width: 300px;\"></a></td>"]) ss, "</tr><tr>",
  --         concat $ map (\s -> concat ["<td><a href=\"", "../", channel1, "--", channel2, "_", s, "-", gps, "-", "32", ".txt\" download=\"",
  --                                   channel1, "--", channel2, "_", s, "-", gps, "-", "32", ".txt\"> Download </a></td>"]) ss,
  --         "</tr></table>"]
                                      
putNames :: String -> [String] -> String -> [String] -> [String] -> String
putNames gps methods channel1 channels2 msgs = concat [
  "<h2>GPS Time: ", gps, "&nbsp; (", (gps2localTime (read gps) "JST"), ")</h2>",
  (concat $ zipWith (putName gps methods channel1) channels2 msgs),
  timeShiftLink "./multiChannelViewer.cgi" gps "1" uris
  ]
  where uris = "&channel1="++channel1
               ++ (concat $ zipWith (++) (repeat "&channel2=") channels2)
               ++ (concat $ zipWith (++) (repeat "&method=") methods)

                 
corMain :: String -> [String] -> String -> String -> IO String
corMain gps methods ch1 ch2 = do
  datMaybe1 <- kagraDataGet (read gps) (read "32") ch1
  datMaybe2 <- kagraDataGet (read gps) (read "32") ch2
  case (datMaybe1, datMaybe2) of
   (Nothing, _) -> return $ "Can't find file or channel-1: "++ch1
   (_, Nothing) -> return $ "Can't find file or channel-2: "++ch2
   (_, _) -> do
     let dat1 = fromJust datMaybe1
         dat2 = fromJust datMaybe2
     fs1 <- liftM fromJust $ (`getSamplingFrequency` ch1) =<< liftM (head.fromJust) (kagraDataFind (read gps) (read "32") ch1)
     fs2 <- liftM fromJust $ (`getSamplingFrequency` ch2) =<< liftM (head.fromJust) (kagraDataFind (read gps) (read "32") ch2)
     forM_ methods $ \method -> do
       pngExits <- doesFileExist $ pngpath++ch1++"--"++ch2++"_"++method++"-"++gps++"-"++"32"++".png"
       case (pngExits, fs1==fs2) of
        (True, _) -> return ()
        (False, False) -> return ()
        (False, True) -> do
          case method of
           "Coh" -> do
             let coh'f = coherenceMon (truncate fs1) fs1 dat1 dat2
             plotV Linear LinePoint 1 BLUE ("[Hz]", "|coh(f)|^2") 0.05 method 
               (pngpath++ch1++"--"++ch2++"_"++method++"-"++gps++"-"++"32.png") ((0,0),(0,0)) $ coh'f
           "Peason" -> do
             let cor = takeCorrelationV (read method) dat1 dat2 16
             plotV Linear LinePoint 1 BLUE ("[sec]", "correlation") 0.05 method 
               (pngpath++ch1++"--"++ch2++"_"++method++"-"++gps++"-"++"32.png") ((0,0),(0,0)) $ (fromList [0,1/fs1..], cor)
           "MIC" -> do
             let cor = takeCorrelationV (read method) dat1 dat2 16
             plotV Linear LinePoint 1 BLUE ("[sec]", "correlation") 0.05 method 
               (pngpath++ch1++"--"++ch2++"_"++method++"-"++gps++"-"++"32.png") ((0,0),(0,0)) $ (fromList [0,1/fs1..], cor)
     return ""
       
body :: Maybe String -> [String] -> Maybe String -> [String] -> String -> String
body gps methods channel1 channels2 script =
  unsafePerformIO $ case (gps, methods ,channel1, channels2) of
                     (Just "", _, _, _) -> do
                       nowGps <- return "1120543424" --getCurrentGps
                       return $ inputForm nowGps script
                     (Just x, [], _, _) -> return $ inputForm x script
                     (Just x, _, Just "", _) -> return $ inputForm x script
                     (Just x, _, _, []) -> return $ inputForm x script
                     (Just x, y, Just z, w)  -> do
                       msgs <- mapM (corMain x y z) w
                       return $ putNames x y z w msgs
                     (_, _, _, _) -> do
                       nowGps <-return "1120543424" --getCurrentGps
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
  channel1 <- getInput "channel1"
  channels2 <- getMultiInput "channel2"
  methods <- getMultiInput "method"
  output $ hkalFrame "Multi Channel Viewer" $ body gps methods channel1 channels2 script

main :: IO ()
main = runCGI (handleErrors cgiMain)
