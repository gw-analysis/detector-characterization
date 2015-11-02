{- |
Module      : frame.cgi
Description : This is documentation tests.
Copyright   : (c) WhoAmI, 2014
License     : ???
Maintainer  : Takahiro Yamamoto %mail%
Stability   : test
Portability : POSIX

-}{-
  * Last Modified: 2015/11/02 18:14:49
-}

import Network.CGI

import HasKAL.TimeUtils.GPSfunction (getCurrentGps, gps2localTimetuple)


main :: IO ()
main = runCGI $ handleErrors cgiMain

cgiMain :: CGI CGIResult
cgiMain = do
  gps <- liftIO getCurrentGps
  subSys <- getInput "subSys"
  let (yyyy, mm, dd, _,_,_,_) = gps2localTimetuple (read gps) "JST"
  output $ html subSys (yyyy, mm, dd)

html :: Maybe String -> (Int, Int, Int) -> String
html subSys (yyyy, mm, dd) = concat [
  "<!DOCTYPE html>",
    "<html>",
      "<head>",
        "<title>HasKAL</title>",
      "</head>",
      "<frameset rows=\"100,*\">",
        "<frame src=\"./dailySubSys.cgi"++str subSys++"\" name=\"portal\">",
          "<frameset cols=\"220,*\">",
            "<frame src=\"./calSubSys.cgi"++str subSys++"\" name=\"calendar\">",
            "<frame src=\""++today++str subSys++"\" name=\"plotframe\">",
          "</frameset>",
      "</frameset>",
  "</html>"
  ]
  where today = "../"++(show yyyy)++"/"++(show0 mm)++"/"++(show0 $ dd - 1)++"/"
                   ++(show yyyy)++"-"++(show0 mm)++"-"++(show0 $ dd - 1)++str subSys++".html"
        str mbx = case mbx of
                   Just "" -> ""
                   Just x  -> "?subSys="++x
                   Nothing -> ""

show0 x
  | x < 10    = "0"++(show x)
  | otherwise = show x

