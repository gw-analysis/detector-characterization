{- |
Module      : CommonForm
Description : This is documentation tests.
Copyright   : (c) WhoAmI, 2014
License     : ???
Maintainer  : Takahiro Yamamoto %mail%
Stability   : test
Portability : POSIX

-}{-
  * Last Modified: 2015/07/15 18:25:40
-}

module CommonForm (
  MultiSelect (Single, Multi, Both),
  hkalFrame,
  channelForm,
  dateForm
  ) where

import HasKAL.TimeUtils.GPSfunction

data MultiSelect = Single | Multi | Both deriving Eq

hkalFrame :: String -> String -> String
hkalFrame title body = concat [
  "<html><head>",
  "<title>", title, "</title>",
  "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\">",
  "</head><body>",
  "<h1>", title, "</h1>",
  body,
  "<br><br><Hr><footer>",
  "<div><p>Real time quick look page: <a href=\"../index.html\">here</a><p>",
  "<a href=\"./pastDataViewer.cgi\">Past Data Viewer</a>",
  "<br><a href=\"./webMonitor.cgi\">Web Monitor</a>",
  "<br><a href=\"./multiChannelViewer.cgi\">Multi Channel Viewer</a></div>",
  "<small>Powerd by <a href=\"https://github.com/gw-analysis\">HasKAL</a></small></footer>",
  "</body></html>"
  ]

channelForm :: MultiSelect -> [String] -> String
channelForm multi chs = concat [
  "<h3> Channel: </h3>",
  case multi of
   Single -> concat [
     "<p><select name=\"channel\" size=\"5\" style=\"font-size:90%; \">",
     concat $ map (\x -> "<option value=\""++x++"\">"++x++"</option>") chs,
     "</select></p>"]
   Multi -> concat [
     "<p><select name=\"channel\" size=\"5\" multiple style=\"font-size:90%; \">",
     concat $ map (\x -> "<option value=\""++x++"\">"++x++"</option>") chs,
     "</select></p>"]
   Both -> concat [
     "<table><tr><th>Channel 1:</th><th></th><th>Channel 2:</th></tr>",
     "<tr><td><select name=\"channel1\" size=\"5\" style=\"font-size:90%; \">",
     concat $ map (\x -> "<option value=\""++x++"\">"++x++"</option>") chs,
     "</select></td><td></td><td><select name=\"channel2\" size=\"5\" multiple style=\"font-size:90%; \">",
     concat $ map (\x -> "<option value=\""++x++"\">"++x++"</option>") chs,
     "</select></td></tr></table>"]
  ]

dateForm :: String -> String
dateForm gps = concat [
  "<h3> Date: </h3>",
  "<p><input type=\"radio\" name=\"Date\" value=\"GPS\" checked=\"checked\" />",
  " GPS Time: <input type=\"text\" name=\"gps\" value=\"", gps, "\" size=\"13\" /></p>",
  "<p><input type=\"radio\" name=\"Date\" value=\"Local\" /> Local Time: ",
  setDef "year" yr [2015..2020], setDef "month" mon [1..12], setDef "day" day [1..31], "&ensp;",
  setDef "hour" hrs [0..23], ":", setDef "minute" min [0..59], ":", setDef "second" sec [0..59], "&ensp;",
  "<select name=\"local\">",
  concat $ map (\x -> "<option value=\""++x++"\" >"++x++"</option>") ["JST", "UTC"],
  "</select></p>"
  ]
  where (yr, mon, day, hrs, min, sec, _) = gps2localTimetuple (read gps) "JST"

setDef :: String -> Int -> [Int] -> String
setDef name x ys =
  "<select name=\""++name++"\">"++
  (concat.(`map` ys) $ \y -> do
      let (v, w) = case name of "month" -> showMonth y
                                _       -> show0 y
      case x==y of True -> "<option value=\""++v++"\" selected>"++w++"</option>"
                   False -> "<option value=\""++v++"\">"++w++"</option>")
  ++"</select>"
                   
show0 :: Int -> (String, String)
show0 x = case (length $ show x) of
  0 -> ("00", "0")
  1 -> ("0"++(show x), show x)
  _ -> (show x, show x)
  
showMonth :: Int -> (String, String)
showMonth x = case x of
  1 -> ("01", "Jan.")
  2 -> ("02", "Feb.")
  3 -> ("03", "Mar.")
  4 -> ("04", "Apr.")
  5 -> ("05", "May")
  6 -> ("06", "Jun.")
  7 -> ("07", "Jul.")
  8 -> ("08", "Aug.")
  9 -> ("09", "Sep.")
  10 -> ("10", "Oct.")
  11 -> ("11", "Nov.")
  12 -> ("12", "Dec.")
  _ -> ("00", "???")
  
