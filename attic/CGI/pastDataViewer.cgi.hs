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
  * Last Modified: 2015/05/13 23:52:08
-}

import Network.CGI

pngpath :: String
pngpath = "../env_images/"

inputForm :: String -> String
inputForm script = concat [
  "<form action=\"", script, "\" method=\"GET\">",
  "<h1>Past Data Viewer</h1>",
  "<p>GPS Time: <input type=\"text\" name=\"gps\" value=\"1115040505\" /></p>",
  "<p> Channel: </p>",
  "<p><input type=\"checkbox\" name=\"channel\" value=\"K1:PEM-EX_ACC_NO2_X_FLOOR\">K1:PEM-EX_ACC_NO2_X_FLOOR</p>",
  "<p><input type=\"checkbox\" name=\"channel\" value=\"K1:PEM-EX_ACC_NO2_Y_FLOOR\">K1:PEM-EX_ACC_NO2_Y_FLOOR</p>",
  "<p><input type=\"checkbox\" name=\"channel\" value=\"K1:PEM-EX_ACC_NO2_Z_FLOOR\">K1:PEM-EX_ACC_NO2_Z_FLOOR</p>",
  "<p><input type=\"checkbox\" name=\"channel\" value=\"K1:PEM-EX_MAG_X_FLOOR\">K1:PEM-EX_MAG_X_FLOOR</p>",
  "<p><input type=\"checkbox\" name=\"channel\" value=\"K1:PEM-EX_MAG_Y_FLOOR\">K1:PEM-EX_MAG_Y_FLOOR</p>",
  "<p><input type=\"checkbox\" name=\"channel\" value=\"K1:PEM-EX_MAG_Z_FLOOR\">K1:PEM-EX_MAG_Z_FLOOR</p>",
  "<p><input type=\"checkbox\" name=\"channel\" value=\"K1:PEM-EX_MIC_FLOOR\">K1:PEM-EX_MIC_FLOOR</p>",    
  "<input type=\"submit\" value=\"view\" />",
  "</form>"]

putName :: String -> String -> String
putName gps channel = concat ["<Hr><h3> Channel: ", channel, "</h3>",
                              "<nobr><a href=\"", pngpath, channel, "_TS-", gps, "-32.png\">",
                              "<img alt=\"\" src=\"", pngpath, channel, "_TS-", gps, "-32.png\"",
                              imgstyle,
                              "<a href=\"", pngpath, channel, "_PSD-", gps, "-32.png\">",
                              "<img alt=\"\" src=\"", pngpath, channel, "_PSD-", gps, "-32.png\"",
                              imgstyle,
                              "<a href=\"", pngpath, channel, "_SPE-", gps, "-32.png\">",
                              "<img alt=\"\" src=\"", pngpath, channel, "_SPE-", gps, "-32.png\"",
                              imgstyle,"</nobr><br><br><br>"
                             ]
  where imgstyle = "style=\"border: 0px solid ; width: 300px;\"></a>"


putNames :: String -> [String] -> String
putNames gps channels = header ++ (concat $ map (putName gps) channels) ++ footer
  where header = concat ["<h1>Past Data Viewer</h1>", "<h2>GPS Time: ", gps, "</h2>"]
        footer = concat ["<Hr>[<a href=\"./pastDataViewer.cgi?gps=", (show $ (read gps) - 32), uriCh, "\">&lt; Prev</a>] ",
                         " [<a href=\"./pastDataViewer.cgi\">Back</a>] ",
                         " [<a href=\"./pastDataViewer.cgi?gps=", (show $ (read gps) + 32), uriCh, "\">Next &gt;</a>]"
                        ]
        uriCh = concat $ zipWith (++) (repeat "&channel=") channels
                 
body :: Maybe String -> [String] -> String -> String
body gps channels script =
    case (gps, channels) of
     (Just "", _) -> inputForm script
     (_, []) -> inputForm script
     (Just x, y)  -> putNames x y
     (_, _) -> inputForm script

html :: Maybe String -> [String] -> String -> String
html gps channels script = concat [
  "<html>",
  "<head><title>Past Data Viewer</title>",
  "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\">",
  "</head>",
  "<body>",
  body gps channels script,
  "</body>",
  "</html>"]

cgiMain :: CGI CGIResult
cgiMain = do
  setHeader "Content-type" "text/html; charset = UTF-8"
  script <- scriptName
  gps <- getInput "gps"
  channels <- getMultiInput "channel"
  output $ html gps channels script

main :: IO ()
main = runCGI (handleErrors cgiMain)
