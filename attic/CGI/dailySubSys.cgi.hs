
import Network.CGI
import HasKAL.TimeUtils.GPSfunction (getCurrentGps, gps2localTimetuple)

main :: IO ()
main = runCGI $ handleErrors cgiMain

cgiMain :: CGI CGIResult
cgiMain = do
  gps <- liftIO getCurrentGps
  subSys <- getInput "subSys"
  [year,month,day] <- mapM getInput ["year", "month", "day"]
  let (yyyy, mm, dd, _,_,_,_) = case (year,month,day) of 
                                 (Just "", _, _) -> gps2localTimetuple (read gps) "JST"
                                 (_, Just "", _) -> gps2localTimetuple (read gps) "JST"
                                 (_, _, Just "") -> gps2localTimetuple (read gps) "JST"
                                 (Just x, Just y, Just z) -> (read x, read y, (read z), 1,1,1,"")
                                 (_, _, _) -> gps2localTimetuple (read gps) "JST"
  output $ html subSys (yyyy, mm, dd)

html :: Maybe String -> (Int, Int, Int) -> String
html subSys date = concat [
  "<html>",
  "<head>",
  "<title>HasKAL Daily Summary</title>",
  "<base target=\"_top\">",
  "</head>",
  "<body>",
  "<div align=center>",
  "<table border=0 cellspacing=0 cellpadding=0>",
  "<tr>",
  concat $ map (subhtml subSys date) [
    "General",
    {- "TUN", "FCL", "VAC", "CRY", -}
    "MIF",
    "VIS", {- "MIR", "LAS", "MIF", -}
    "IOO", {- "AOS", "AEL", "DGS", -}
    {- "DMG", "DAS", "GIF", "DEC"  -}
    "ENV",
    "Bruco"],
  "<td align=center valign=middle width=98 height=30>",
  "<div align=center><a href=\"webToolFrame.cgi\">Web Tools</a></div>",
  "</td>",
  "</tr>",
  "</table>",
  "</div>",
  "</body>",
  "</html>"
  ]

subhtml :: Maybe String -> (Int, Int, Int) -> String -> String
subhtml subSys date sys = concat [
  "<td align=center valign=middle width=98 height=30 "++color subSys sys++">",
  "<div align=center><a href=\"./dailyFrame.cgi?subSys="++sys++uri date++"\">"++sys++"</a></div>",
  "</td>"
  ]
  where color subSys sys = case subSys of
                            Just x -> case x==sys of
                                       True -> "bgcolor=\"#ccffff\""
                                       False -> ""
                            Nothing -> case sys=="General" of
                                        True -> "bgcolor=\"#ccffff\""
                                        False -> ""
        uri (yyyy, mm, dd) = "&year="++(show yyyy)++"&month="++(show0 mm)++"&day="++(show0 dd)

show0 x
  | x < 10    = "0"++(show x)
  | otherwise = show x
