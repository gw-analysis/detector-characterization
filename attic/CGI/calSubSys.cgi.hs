
import Network.CGI
import Data.Maybe

import HasKAL.TimeUtils.GPSfunction (getCurrentGps, gps2localTimetuple)

startDate = (2012, 1)

main :: IO ()
main = runCGI $ handleErrors cgiMain

cgiMain :: CGI CGIResult
cgiMain = do
  gps <- liftIO getCurrentGps
  subSys <- getInput "subSys"
  let (yyyy, mm, dd, _,_,_,_) = gps2localTimetuple (read gps) "JST"
  output $ htmlcal subSys (yyyy,mm,dd)

htmlcal :: Maybe String -> (Int, Int, Int) -> String
htmlcal subSys (yy, mm, dd) = concat [
  "<html>",
    "<head>",
      "<title>Calendar</title>",
    "</head>",
    "<body>",
      "<h1>Calendar</h1>",
      concat $ map (htmlcalCore subSys (yy, mm, dd)) $ reverse dateRange,
    "</body>",
  "</html>"
  ]
  where dateRange = map modDate $ zip (repeat $ fst startDate) $ take (lenMonth (yy, mm)) [snd startDate..]

modDate :: (Int, Int) -> (Int, Int)
modDate (yyyy, mm) = (yyyy+(num quotient rest), rest)
  where rest = mod mm 12
        quotient = mm`div`12
        num x 0 = (x-1)
        num x _ = x

lenMonth :: (Int, Int) -> Int
lenMonth (y2, m2) = 12*(y2-y1) + (m2-m1) + 1
  where (y1, m1) = startDate


htmlcalCore :: Maybe String -> (Int, Int, Int) -> (Int, Int) -> String
htmlcalCore subSys today (yy, mm) = concat [
  "<table>",
    "<tr><th>"++(monthName mm)++" "++(show yy)++"</th></tr>",
    "<tr><th>",
      "<table style=\"font-size:80%;\">",
        "<tr align=\"right\"><td>Mon.</td><td>Tue.</td><td>Wed.</td><td>Thu.</td><td>Fri.</td><td>Sat.</td><td>Sun.</td></tr>",
          monthlyLink subSys today (yy, mm),
      "</table>",
    "</th></tr>",
  "</table><br>"
  ]

monthlyLink :: Maybe String -> (Int, Int, Int) -> (Int, Int) -> String
monthlyLink subSys today (yyyy, mm) = concat $ newline $ map (dailyLink subSys today) dateList
  where dateList = zip3 (replicate (length ddList) yyyy) (replicate (length ddList) mm) ddList
        ddList = (replicate (day-1) 0) ++ [1..lastDay (yyyy, mm)] ++ (replicate (35-(lastDay (yyyy,mm))-(day-1)) 0)
        day = zeller (yyyy, mm, 1)
        newline xs = map ((\str->"<tr>"++str++"</tr>").concat.(\i -> (take 7).(drop $ i*7) $ xs)) [0..length xs`div`7]
        
       

zeller :: (Int, Int, Int) -- ^ Gregorian
       -> Int             -- ^ ISO 8601
zeller (yyyy, mm, dd) = h + 1
  where h = mod (dd' + (26*(mm'+1)`div`10) + yCapital + (yCapital`div`4) + gammaCapital + 5) 7
        yCapital = mod yyyy' 100
        gammaCapital = (cCapital`div`4) - 2*cCapital
        cCapital = yyyy' `div` 100
        (yyyy', mm', dd') = zellerDateCore (yyyy, mm, dd)

zellerDateCore :: (Int, Int, Int) -> (Int, Int, Int)
zellerDateCore (yyyy, mm, dd)
  | mm == 1   = (yyyy-1, 13, dd)
  | mm == 2   = (yyyy-1, 14, dd)
  | otherwise = (yyyy, mm, dd)

dailyLink :: Maybe String -> (Int, Int, Int) -> (Int, Int, Int) -> String
dailyLink subSys today (yyyy, mm, dd) 
  | dd == 0                 = "<td></td>"
  | (yyyy, mm, dd) >= today = "<td align=\"right\">"++(show dd)++"&ensp;</td>"
  | otherwise               = "<td align=\"right\"><a href=\""++linkfile++"\" target=\"plotframe\">"++(show dd)++"</a>&ensp;</td>"
  where linkfile = "../"++(show yyyy)++"/"++(show0 mm)++"/"++(show0 dd)++"/"
                   ++(show yyyy)++"-"++(show0 mm)++"-"++(show0 dd)++str subSys++".html"
        str mbx = case mbx of 
                   (Just "") -> ""
                   (Just x)  -> "_"++x
                   Nothing   -> ""

lastDay :: (Int, Int) -> Int
lastDay (yy, mm) 
  | mm == 0 = 31 -- Dec
  | mm == 1 = 31
  | mm == 2 && (yy`div`400)==0 = 29
  | mm == 2 && (yy`div`100)==0 = 28
  | mm == 2 && (yy`div`4)==0   = 29
  | mm == 2 = 28
  | mm == 3 = 31
  | mm == 4 = 30
  | mm == 5 = 31
  | mm == 6 = 30
  | mm == 7 = 31
  | mm == 8 = 31
  | mm == 9 = 30
  | mm == 10 = 31
  | mm == 11 = 30
  | mm == 12 = 31

monthName :: Int -> String
monthName mm
  | mm == 0 = "Dec."
  | mm == 1 = "Jan."
  | mm == 2 = "Feb."
  | mm == 3 = "Mar."
  | mm == 4 = "Apr."
  | mm == 5 = "May"
  | mm == 6 = "Jun."
  | mm == 7 = "Jul."
  | mm == 8 = "Aug."
  | mm == 9 = "Sep."
  | mm == 10 = "Oct."
  | mm == 11 = "Nov."
  | mm == 12 = "Dec."


show0 x
  | x < 10    = "0"++(show x)
  | otherwise = show x
