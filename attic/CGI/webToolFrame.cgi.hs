
import Network.CGI

import HasKAL.TimeUtils.GPSfunction (getCurrentGps, gps2localTimetuple)
import SampleChannel

main :: IO ()
main = runCGI $ handleErrors cgiMain

cgiMain :: CGI CGIResult
cgiMain = do
  gps <- return "1134572417" --liftIO getCurrentGps
  portal <- getInput "portal"
  output $ html portal gps

html :: Maybe String -> String -> String
html portal gps = concat [
  "<!DOCTYPE html>",
    "<html>",
      "<head>",
        "<title>HasKAL</title>",
      "</head>",
      "<frameset rows=\"100,*\">",
        "<frame src=\"./webToolPortal.cgi"++uri portal++"\" name=\"portal\">",
          "<frameset cols=\"310,*\">",
          "<frame src=\""++(func portal)++"\" name=\"input\">",
          "<frame src=\""++(func2 portal gps)++"\" name=\"plotframe\">",
          "</frameset>",
      "</frameset>",
  "</html>"
  ]
  where uri mbx = case mbx of
                   Just "" -> "?portal=Single Channel Analysis"
                   Just x  -> "?portal="++x++"&"
                   Nothing -> "?"
            
        func :: Maybe String -> String
        func portal = case portal of
                       Just "Single Channel Analysis" -> "date_1.cgi"
                       Just "Coherence Analysis"      -> "date_2.cgi"
                       Just "Correlation Map"         -> "date_3.cgi"
                       Just "Bruco"                   -> "date_4.cgi"
                       Just "Detection Range"         -> "date_5.cgi"
                       Nothing                        -> "date_1.cgi"


        func2 :: Maybe String -> String ->  String
        func2 portal gps = case portal of
                            Just "Single Channel Analysis" -> "date_1.cgi?Date=GPS&gps="++gps++"&channel1="++ch1
                                                                ++"&monitor=TS\" name=\"plotframe\">"
                            Just "Coherence Analysis"      -> "date_2.cgi?Date=GPS&gps="++gps++"&channel1="++ch1
                                                                ++"&channel2="++ch2++"&monitor=COH\" name=\"plotframe\">"
                            Just "Correlation Map"         -> "date_3.cgi?Date=GPS&gps="++gps++"&channel1="++ch1
                                                                ++"&channel1="++ch2++"&monitor=Pearson\" name=\"plotframe\">"
                            Just "Bruco"                   -> "" 
                            Just "Detection Range"         -> "date_5.cgi?Date=GPS&gps="++gps
                                                           ++"&monitor=INSP\" name=\"plotframe\">"
                            Nothing                        -> "date_1.cgi?Date=GPS&gps="++gps++"&channel1="++ch1 
                                                                ++"&monitor=TS\" name=\"plotframe\">"            
        ch1 = defaultChs!!0
        ch2 = defaultChs!!1


