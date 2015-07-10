{- |
Module      : CommonForm
Description : This is documentation tests.
Copyright   : (c) WhoAmI, 2014
License     : ???
Maintainer  : Takahiro Yamamoto %mail%
Stability   : test
Portability : POSIX
GUI of Antenna Pattern
-}{-
  * Last Modified: 2015/07/10 22:08:15
-}

module CommonForm (
  hkalFrame,
  channelForm,
  dateForm
  ) where

hkalFrame :: String -> String -> String
hkalFrame title body = concat [
  "<html><head>",
  "<title>", title, "</title>",
  "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\">",
  "</head><body>",
  "<h1>", title, "</h1>",
  body,
  "<br><br><Hr><footer><p>Real time quick look page: <a href=\"../index.html\">here</a><br><p>",
  "<small>Powerd by <a href=\"https://github.com/gw-analysis\">HasKAL</a></small></footer>",
  "</body></html>"
  ]

channelForm :: [String] -> String
channelForm chs = concat [
  "<h3> Channel: </h3>",
  "<p><select name=\"channel\" size=\"5\" multiple style=\"font-size:90%;background-color:#eeeeee; \">",
  concat $ map (\x -> "<option value=\""++x++"\">"++x++"</option>") chs,
  "</select></p>"
  ]

dateForm :: String
dateForm = concat [
  "<h3> Date: </h3>",
  "<p><input type=\"radio\" name=\"Date\" value=\"GPS\" checked=\"checked\" />",
  " GPS Time: <input type=\"text\" name=\"gps\" value=\"1115097401\" size=\"13\" /></p>",
  "<p><input type=\"radio\" name=\"Date\" value=\"Local\" /> Local Time: ",
  "<select name=\"year\">",
  concat $ map (\x -> "<option value=\""++x++"\" >"++x++"</option>") $ map show [2015..2020],
  "</select>",
  "<select name=\"month\">",
  concat $ map (\(x, y) -> "<option value=\""++x++"\" >"++y++"</option>") $ map showMonth [1..12],
  -- "<option value=\"05\" selected>May</option>",
  "</select>",
  "<select name=\"day\">",
  concat $ map (\x -> "<option value=\""++x++"\" >"++x++"</option>") $ map show0 [1..31],
  -- "<option value=\"08\" selected>8</option>",
  "</select> ",
  "<select name=\"hour\">",
  concat $ map (\x -> "<option value=\""++x++"\" >"++x++"</option>") $ map show0 [0..23],
  -- "<option value=\"14\" selected>14</option>",
  "</select>:",
  "<select name=\"minute\">",
  concat $ map (\x -> "<option value=\""++x++"\" >"++x++"</option>") $ map show0 [0..59],
  -- "<option value=\"16\" selected>16</option>",
  "</select>:",
  "<select name=\"second\">",
  concat $ map (\x -> "<option value=\""++x++"\" >"++x++"</option>") $ map show0 [0..59],
  -- "<option value=\"25\" selected>25</option>",
  "</select>",
  "<select name=\"local\">",
  concat $ map (\x -> "<option value=\""++x++"\" >"++x++"</option>") ["JST", "UTC"],
  "</select>",
  "</p>"
  ]

show0 :: Int -> String
show0 x = case (length $ show x) of
  0 -> "00"
  1 -> "0"++(show x)
  _ -> show x
  
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
  
