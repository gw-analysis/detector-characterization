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
  * Last Modified: 2015/05/07 23:34:04
-}

import Network.CGI

inputForm :: String -> String
inputForm script = concat [
  "<form action=\"", script, "\" method=\"GET\">",
  "<h1>Past Data Viewer</h1>",
  "<p>GPS Time: <input type=\"text\" name=\"gps\" value=\"1115040505\" /></p>",
  "<p> Channel: </p>",
  "<p><input type=\"radio\" name=\"channel\" value=\"K1:PEM-EX_ACC_NO2_X_FLOOR\">K1:PEM-EX_ACC_NO2_X_FLOOR</p>",
  "<p><input type=\"radio\" name=\"channel\" value=\"K1:PEM-EX_ACC_NO2_Y_FLOOR\">K1:PEM-EX_ACC_NO2_Y_FLOOR</p>",
  "<p><input type=\"radio\" name=\"channel\" value=\"K1:PEM-EX_ACC_NO2_Z_FLOOR\">K1:PEM-EX_ACC_NO2_Z_FLOOR</p>",
  "<p><input type=\"radio\" name=\"channel\" value=\"K1:PEM-EX_MAG_X_FLOOR\">K1:PEM-EX_MAG_X_FLOOR</p>",
  "<p><input type=\"radio\" name=\"channel\" value=\"K1:PEM-EX_MAG_Y_FLOOR\">K1:PEM-EX_MAG_Y_FLOOR</p>",
  "<p><input type=\"radio\" name=\"channel\" value=\"K1:PEM-EX_MAG_Z_FLOOR\">K1:PEM-EX_MAG_Z_FLOOR</p>",
  "<p><input type=\"radio\" name=\"channel\" value=\"K1:PEM-EX_MIC_FLOOR\">K1:PEM-EX_MIC_FLOOR</p>",    
  "<input type=\"submit\" value=\"view\" />",
  "</form>"]

putName :: String -> String -> String
putName gps channel = concat ["<p>GPS Time: ", gps, "</p>",
                              "<p> Channel: ", channel, "</p>",
                              "<a href=\"", pngpath, channel, "_TS-", gps, "-32.png\">",
                              "<img alt=\"\" src=\"", pngpath, channel, "_TS-", gps, "-32.png\"",
                              "style=\"border: 0px solid ; width: 300px;\"></a>",
                              "<a href=\"", pngpath, channel, "_PSD-", gps, "-32.png\">",
                              "<img alt=\"\" src=\"", pngpath, channel, "_PSD-", gps, "-32.png\"",
                              "style=\"border: 0px solid ; width: 300px;\"></a>",
                              "<a href=\"", pngpath, channel, "_SPE-", gps, "-32.png\">",
                              "<img alt=\"\" src=\"", pngpath, channel, "_SPE-", gps, "-32.png\"",
                              "style=\"border: 0px solid ; width: 300px;\"></a>"
                             ]
  where pngpath = "png/"

body :: Maybe String -> Maybe String -> String -> String
body gps channel script =
    case (gps, channel) of
     (Just "", _) -> inputForm script
     (_, Just "") -> inputForm script     
     (Just x, Just y)  -> putName x y
     (_, _) -> inputForm script

html :: Maybe String -> Maybe String -> String -> String
html gps channel script = concat [
  "<html>",
  "<head><title>Past Data Viewer</title>",
  "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\">",
  "</head>",
  "<body>",
  body gps channel script,
  "</body>",
  "</html>"]

cgiMain :: CGI CGIResult
cgiMain = do
  setHeader "Content-type" "text/html; charset = UTF-8"
  script <- scriptName
  gps <- getInput "gps"
  channel <- getInput "channel"
  output $ html gps channel script

main :: IO ()
main = runCGI (handleErrors cgiMain)
