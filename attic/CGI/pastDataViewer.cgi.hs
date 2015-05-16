{- |
Module      : pastDataViewer.cgi
Description : This is documentation tests.
Copyright   : (c) WhoAmI, 2014
License     : ???
Maintainer  : Takahiro Yamamoto %mail%
Stability   : test
Portability : POSIX
GUI of Antenna Pattern
-}{-
  * Last Modified: 2015/05/16 19:03:47
-}

import Network.CGI

pngpath :: String
pngpath = "../env_images/"

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
  "<h3> Type: </h3>",
  "<p><input type=\"checkbox\" name=\"plottype\" value=\"TS\" checked=\"checked\">Time Series</p>",
  "<p><input type=\"checkbox\" name=\"plottype\" value=\"PSD\" checked=\"checked\">Spectrum</p>",
  "<p><input type=\"checkbox\" name=\"plottype\" value=\"SPE\" checked=\"checked\">Spectrogram</p>",
  "<input type=\"submit\" value=\"plot view\" />",
  "</form>"]

putName :: String -> String -> [String] -> String
putName gps channel plottypes = concat ["<Hr><h3> Channel: ", channel, "</h3>"] ++ (concat $ map func plottypes) ++ "<br><br><br>"
  where imgstyle = "style=\"border: 0px solid ; width: 300px;\"></a>"
        func s = concat ["<nobr><a href=\"", pngpath, channel, "_", s, "-", gps, "-32.png\">",
                         "<img alt=\"\" src=\"", pngpath, channel, "_", s, "-", gps, "-32.png\"",
                         imgstyle,"</nobr>"]


putNames :: String -> [String] -> [String] -> String
putNames gps channels plottypes = header ++ (concat $ map (\x -> (putName gps) x plottypes) channels) ++ footer
  where header = concat ["<h2>GPS Time: ", gps, "</h2>"]
        footer = concat ["<Hr>[<a href=\"./pastDataViewer.cgi?gps=", (show $ (read gps) - 32), uriCh, uriPt, "\">&lt; Prev</a>] ",
                         " [<a href=\"./pastDataViewer.cgi\">Back</a>] ",
                         " [<a href=\"./pastDataViewer.cgi?gps=", (show $ (read gps) + 32), uriCh, uriPt, "\">Next &gt;</a>]"
                        ]
        uriCh = concat $ zipWith (++) (repeat "&channel=") channels
        uriPt = concat $ zipWith (++) (repeat "&plottype=") plottypes
                 
body :: Maybe String -> [String] -> [String] -> String -> String
body gps channels plottypes script =
    case (gps, channels, plottypes) of
     (Just "", _, _) -> inputForm script
     (_, [], _) -> inputForm script
     (_, _, []) -> inputForm script
     (Just x, y, z)  -> putNames x y z
     (_, _, _) -> inputForm script

html :: Maybe String -> [String] -> [String] -> String -> String
html gps channels plottypes script = concat [
  "<html>",
  "<head><title>Past Data Viewer</title>",
  "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\">",
  "</head><body>",
  "<h1>Past Data Viewer</h1>",
  body gps channels plottypes script,
  "<br><br><Hr><footer><p>Real time quick look page: <a href=\"../index.html\">here</a><br><p>",
  "<small>Powerd by <a href=\"https://github.com/gw-analysis\">HasKAL</a></small></footer>",
  "</body></html>"]

cgiMain :: CGI CGIResult
cgiMain = do
  setHeader "Content-type" "text/html; charset = UTF-8"
  script <- scriptName
  gps <- getInput "gps"
  channels <- getMultiInput "channel"
  plottypes <- getMultiInput "plottype"
  output $ html gps channels plottypes script



main :: IO ()
main = runCGI (handleErrors cgiMain)
