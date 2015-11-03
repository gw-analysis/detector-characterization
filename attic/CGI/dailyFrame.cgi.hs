
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
                                 (Just x, Just y, Just z) -> (read x, read y, (read z)+1, 1,1,1,"")
                                 (_, _, _) -> gps2localTimetuple (read gps) "JST"
  output $ html subSys (yyyy, mm, dd)

html :: Maybe String -> (Int, Int, Int) -> String
html subSys (yyyy, mm, dd) = concat [
  "<!DOCTYPE html>",
    "<html>",
      "<head>",
        "<title>HasKAL</title>",
      "</head>",
      "<frameset rows=\"100,*\">",
        "<frame src=\"./dailySubSys.cgi"++uri subSys++"\" name=\"portal\">",
          "<frameset cols=\"220,*\">",
            "<frame src=\"./calSubSys.cgi"++uri subSys++"\" name=\"calendar\">",
            "<frame src=\""++today++"\" name=\"plotframe\">",
          "</frameset>",
      "</frameset>",
  "</html>"
  ]
  where today = "../"++(show yyyy)++"/"++(show0 mm)++"/"++(show0 $ dd - 1)++"/"
                   ++(show yyyy)++"-"++(show0 mm)++"-"++(show0 $ dd - 1)++str subSys++".html"
        
        str mbx = case mbx of
                   Just "" -> "_General"
                   Just x  -> "_"++x
                   Nothing -> "_General"
        uri mbx = case mbx of
                   Just "" -> "?subSys=General"++date
                   Just x  -> "?subSys="++x++"&"++date
                   Nothing -> "?"++date
                  where date="year="++(show yyyy)++"&month="++(show0 mm)++"&day="++(show0 $ dd - 1)

show0 x
  | x < 10    = "0"++(show x)
  | otherwise = show x

