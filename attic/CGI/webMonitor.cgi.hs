{- |
Module      : webMonitor.cgi
Description : This is documentation tests.
Copyright   : (c) WhoAmI, 2014
License     : ???
Maintainer  : Takahiro Yamamoto %mail%
Stability   : test
Portability : POSIX
GUI of Antenna Pattern
-}{-
  * Last Modified: 2015/05/16 19:18:19
-}

import Network.CGI
import Control.Monad (forM_)
import System.Directory (doesFileExist)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Vector.Storable as V (concat)

import HasKAL.SpectrumUtils.SpectrumUtils
import HasKAL.PlotUtils.HROOT.PlotGraph
import HasKAL.SpectrumUtils.Function
import HasKAL.FrameUtils.Function
import HasKAL.MonitorUtils.SRMon.StudentRayleighMon
import HasKAL.MonitorUtils.RayleighMon.RayleighMon

pngpath :: String
pngpath = "../mon_images/"

inputForm :: String -> String
inputForm script = concat [
  "<form action=\"", script, "\" method=\"GET\">",
  "<p>GPS Time: <input type=\"text\" name=\"gps\" value=\"1115097401\" /></p>",
  "<h3> Channel: </h3>",
  "<p><input type=\"checkbox\" name=\"channel\" value=\"K1:PEM-EX_ACC_NO2_X_FLOOR\">K1:PEM-EX_ACC_NO2_X_FLOOR</p>",
  "<p><input type=\"checkbox\" name=\"channel\" value=\"K1:PEM-EX_ACC_NO2_Y_FLOOR\">K1:PEM-EX_ACC_NO2_Y_FLOOR</p>",
  "<p><input type=\"checkbox\" name=\"channel\" value=\"K1:PEM-EX_ACC_NO2_Z_FLOOR\">K1:PEM-EX_ACC_NO2_Z_FLOOR</p>",
  "<p><input type=\"checkbox\" name=\"channel\" value=\"K1:PEM-EX_MAG_X_FLOOR\">K1:PEM-EX_MAG_X_FLOOR</p>",
  "<p><input type=\"checkbox\" name=\"channel\" value=\"K1:PEM-EX_MAG_Y_FLOOR\">K1:PEM-EX_MAG_Y_FLOOR</p>",
  "<p><input type=\"checkbox\" name=\"channel\" value=\"K1:PEM-EX_MAG_Z_FLOOR\">K1:PEM-EX_MAG_Z_FLOOR</p>",
  "<p><input type=\"checkbox\" name=\"channel\" value=\"K1:PEM-EX_MIC_FLOOR\">K1:PEM-EX_MIC_FLOOR</p>",
  "<h3> Monitor: </h3>",
  "<p><input type=\"checkbox\" name=\"monitor\" value=\"RM\">RayleighMon</p>",
  "<p><input type=\"checkbox\" name=\"monitor\" value=\"SRM\">StudentRayleighMon</p>",
  "<input type=\"submit\" value=\"view\" />",
  "</form>"]

putName :: String -> String -> [String] -> String
putName gps channel monitors = concat ["<Hr><h3> Channel: ", channel, "</h3>"] ++ (concat $ map func monitors) ++ "<br><br><br>"
  where imgstyle = "style=\"border: 0px solid ; width: 300px;\"></a>"
        func s = concat ["<nobr><a href=\"", pngpath, channel, "_", s, "-", gps, "-128.png\">",
                         "<img alt=\"\" src=\"", pngpath, channel, "_", s, "-", gps, "-128.png\"",
                         imgstyle,"</nobr>"]


putNames :: String -> [String] -> [String] -> String
putNames gps channels monitors = header ++ (concat $ map (\x -> (putName gps) x monitors) channels) ++ footer
  where header = concat ["<h2>GPS Time: ", gps, "</h2>"]
        footer = concat ["<Hr>[<a href=\"./webMonitor.cgi?gps=", (show $ (read gps) - 32), uriCh, uriMo, "\">&lt; Prev</a>] ",
                         " [<a href=\"./webMonitor.cgi\">Back</a>] ",
                         " [<a href=\"./webMonitor.cgi?gps=", (show $ (read gps) + 32), uriCh, uriMo, "\">Next &gt;</a>]"
                        ]
        uriCh = concat $ zipWith (++) (repeat "&channel=") channels
        uriMo = concat $ zipWith (++) (repeat "&monitor=") monitors
                 
body :: Maybe String -> [String] -> [String] -> String -> String
body gps channels monitors script =
    unsafePerformIO $ case (gps, channels, monitors) of
                       (Just "", _, _) -> return $ inputForm script
                       (_, [], _) -> return $ inputForm script
                       (_, _, []) -> return $ inputForm script
                       (Just x, y, z)  -> do
                         mapM_ (monMain x y) z
                         return $ putNames x y z
                       (_, _, _) -> return $ inputForm script

html :: Maybe String -> [String] -> [String] -> String -> String
html gps channels monitors script = concat [
  "<html>",
  "<head><title>Web Monitor</title>",
  "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\">",
  "</head><body>",
  "<h1>Web Monitor</h1>",  
  body gps channels monitors script,
  "<br><br><Hr><footer><p>Real time quick look page: <a href=\"../index.html\">here</a><br><p>",
  "<small>Powerd by <a href=\"https://github.com/gw-analysis\">HasKAL</a></small></footer>",
  "</body></html>"]

cgiMain :: CGI CGIResult
cgiMain = do
  setHeader "Content-type" "text/html; charset = UTF-8"
  script <- scriptName
  gps <- getInput "gps"
  channels <- getMultiInput "channel"
  monitors <- getMultiInput "monitor"  
  output $ html gps channels monitors script

monMain :: String -> [String] -> String -> IO ()
monMain gps chs mon
  | mon == "SRM" = srmMain gps chs
  | mon == "RM"  = rmMain gps chs

srmMain :: String -> [String] -> IO ()
srmMain gps chs = do
  forM_ chs $ \ch -> do
    pngExist <- doesFileExist $ pngpath++ch++"_SRM-"++gps++"-128.png"
    gwfExist <- doesFileExist $ "/data/kagra/xend/R0202/K-K1_R-"++gps++"-32.gwf"
    case (pngExist, gwfExist) of
     (False, True) -> do
       let gpss = map show [(read gps), (read gps)+32..(read gps)+96]
       dats <- mapM (\x -> readFrameV ch $ "/data/kagra/xend/R0202/K-K1_R-"++x++"-32.gwf") gpss
       let dat = V.concat dats
       let snf = gwpsdV dat 512 2048
           hfs = gwspectrogramV 0 512 2048 dat
           nus = studentRayleighMonV (QUANT 0.95) 2048 512 256 256 4 snf hfs
       plotV Linear LinePoint 1 BLUE ("[Hz]", "nu") 0.05 "srmon" (pngpath++ch++"_SRM-"++gps++"-128.png") ((0,0),(0,0)) (getSpectrum 0 nus)
     (_,_) -> return ()
    
rmMain :: String -> [String] -> IO ()
rmMain gps chs = do
  forM_ chs $ \ch -> do
    pngExist <- doesFileExist $ pngpath++ch++"_RM-"++gps++"-128.png"
    gwfExist <- doesFileExist $ "/data/kagra/xend/R0202/K-K1_R-"++gps++"-32.gwf"
    case (pngExist, gwfExist) of
     (False, True) -> do
       let gpss = map show [(read gps), (read gps)+32..(read gps)+96]
       dats <- mapM (\x -> readFrameV ch $ "/data/kagra/xend/R0202/K-K1_R-"++x++"-32.gwf") gpss
       let dat = V.concat dats
       let snf = gwpsdV dat 512 2048
           hfs = gwspectrogramV 0 512 2048 dat
           qv = rayleighMonV [0.5, 0.9, 0.95, 0.99] 2048 512 4 snf hfs
       oPlotV Linear LinePoint 1 [RED, RED, BLUE, BLUE, PINK, PINK, GREEN, GREEN] ("[Hz]", "Normalized Noise Lv.") 0.05 "rmon" (pngpath++ch++"_RM-"++gps++"-128.png") ((0,0),(0,6)) (concat $ map pair2list qv)
     (_,_) -> return ()

pair2list :: (a, a) -> [a]
pair2list (x,y) = [x,y]

main :: IO ()
main = runCGI (handleErrors cgiMain)
