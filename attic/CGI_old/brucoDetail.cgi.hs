{- |
Module      : brucoDetail.cgi
Description : This is documentation tests.
Copyright   : (c) WhoAmI, 2014
License     : ???
Maintainer  : Takahiro Yamamoto %mail%
Stability   : test
Portability : POSIX

-}{-
  * Last Modified: 2015/07/21 21:43:32
-}

import Network.CGI
import Control.Monad (forM_, liftM)
import Data.Maybe (fromJust)
import System.Directory (doesFileExist)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Vector.Storable as V (concat)

import HasKAL.MonitorUtils.CoherenceMon.Function
import HasKAL.SpectrumUtils.SpectrumUtils
import HasKAL.FrameUtils.FrameUtils
import HasKAL.PlotUtils.HROOT.PlotGraph
import HasKAL.SpectrumUtils.Function
import HasKAL.TimeUtils.GPSfunction
import HasKAL.DataBaseUtils.Function
import CommonForm

pngpath :: String
pngpath = "../mon_images/"

putNames :: String -> String -> String -> String -> String -> String -> String
putNames gps duration channel1 channel2 freq msg = concat [
  "<h2> GPS Time: ", gps, "&nbsp; (", (gps2localTime (read gps) "JST"), ")</h2>",
  "<ul><li> duration: "++duration++" sec.</li>",
  "<li>frequency: "++freq++" &plusmn; 20 Hz</li>",
  "<li>  Channel: "++channel1++" vs "++channel2++"</li></ul>",
  case msg of "" -> concat ["<nobr><a href=\"", pngpath, channel1, "_PSD-", gps, "-", duration, "-", freq, ".png\">",
                            "<img alt=\"\" src=\"", pngpath, channel1, "_PSD-", gps, "-", duration, "-", freq, ".png\"",
                            "style=\"border: 0px solid ; width: 300px;\"></a></nobr>",
                            "<nobr><a href=\"", pngpath, channel2, "_PSD-", gps, "-", duration, "-", freq, ".png\">",
                            "<img alt=\"\" src=\"", pngpath, channel2, "_PSD-", gps, "-", duration, "-", freq, ".png\"",
                            "style=\"border: 0px solid ; width: 300px;\"></a></nobr>",
                            "<nobr><a href=\"", pngpath, channel1, "__", channel2, "_COH-", gps, "-", duration, "-", freq, ".png\">",
                            "<img alt=\"\" src=\"", pngpath, channel1, "__", channel2, "_COH-", gps, "-", duration, "-", freq, ".png\"",
                            "style=\"border: 0px solid ; width: 300px;\"></a></nobr>"
                           ]
              _  -> "<h4 style=\"color:#ff0000;\">&emsp;"++msg++"</h4>",
  timeShiftLink "./brucoDetail.cgi" gps duration uris
  ]
  where uris = "&channel1="++channel1
               ++"&channel2="++channel2++"&freq="++freq
                 
monMain :: String -> String -> String -> String -> String -> IO String
monMain gps duration ch1 ch2 freq = do
  datMaybe1 <- kagraDataGet (read gps) (read duration) ch1
  datMaybe2 <- kagraDataGet (read gps) (read duration) ch2
  case (datMaybe1, datMaybe2) of
   (Nothing, _) -> return $ "Can't find file or channel: "++ch1
   (_, Nothing) -> return $ "Can't find file or channel: "++ch2
   (_, _) -> do
     let dat1 = fromJust datMaybe1
     let dat2 = fromJust datMaybe2
     fs <- liftM fromJust $ (`getSamplingFrequency` ch1) =<< liftM (head.fromJust) (kagraDataFind (read gps) (read duration) ch1)
     png1Exist <- doesFileExist $ pngpath++ch1++"_PSD-"++gps++"-"++duration++"-"++freq++".png"
     case png1Exist of
      True -> return ()
      False -> do
        let hf1 = gwpsdV dat1 (truncate fs) fs
        plotV LogY LinePoint 1 BLUE ("[Hz]", "/Hz") 0.05 ch1
          (pngpath++ch1++"_PSD-"++gps++"-"++duration++"-"++freq++".png") ((read freq - 20,read freq + 20),(0,0)) hf1
     png2Exist <- doesFileExist $ pngpath++ch2++"_PSD-"++gps++"-"++duration++"-"++freq++".png"
     case png2Exist of
      True -> return ()
      False -> do
        let hf2 = gwpsdV dat2 (truncate fs) fs
        plotV LogY LinePoint 1 BLUE ("[Hz]", "/Hz") 0.05 ch2
          (pngpath++ch2++"_PSD-"++gps++"-"++duration++"-"++freq++".png") ((read freq - 20,read freq + 20),(0,0)) hf2
     png3Exist <- doesFileExist $ pngpath++ch1++"__"++ch2++"_COH-"++gps++"-"++duration++"-"++freq++".png"
     case png3Exist of
      True -> return ()
      False -> do
        let coh = coherenceMon (truncate fs) fs dat1 dat2
        plotV Linear LinePoint 1 BLUE ("[Hz]", "|coh(f)|^2") 0.05 (ch1++" vs "++ch2)
          (pngpath++ch1++"__"++ch2++"_COH-"++gps++"-"++duration++"-"++freq++".png") ((read freq - 20,read freq + 20),((-0.05),1.05)) coh
     return ""

body :: Maybe String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> String -> String
body gps duration channel1 channel2 freq script =
  unsafePerformIO $ case (gps, duration, channel1, channel2, freq) of
                     (Just "", _, _, _, _) -> return "Link error 1"
                     (_, Just "", _, _, _) -> return "Link error 2"
                     (_, _, Just "", _, _) -> return "Link error 3"
                     (_, _, _, Just "", _) -> return "Link error 4"
                     (_, _, _, _, Just "") -> return "Link error 5"
                     (Just x, Just y, Just z, Just v, Just w)  -> do
                       msg <- monMain x y z v w
                       return $ putNames x y z v w msg
                     (_, _, _, _, _) -> return "Link error 6"

cgiMain :: CGI CGIResult
cgiMain = do
  setHeader "Content-type" "text/html; charset = UTF-8"
  script <- scriptName
  gps <- getInput "gps" 
  duration <- getInput "duration" 
  channel1 <- getInput "channel1"
  channel2 <- getInput "channel2"
  freq <- getInput "freq"  
  output $ hkalFrame "Bruco Detail" $ body gps duration channel1 channel2 freq script

main :: IO ()
main = runCGI (handleErrors cgiMain)

