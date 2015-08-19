{-
functions for GPS time
-}


module HasKAL.TimeUtils.GPSfunction
( time2gps
, gps2time
, gps2localTime
, timetuple2gps
, gps2timetuple
, gps2localTimetuple
, mjd2gps'
, getCurrentLocalTime
, getCurrentGps
, gps2unix
) where

import Data.Time
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Clock.TAI
import Data.Time.Clock.POSIX
import System.Environment
import Data.List
import System.IO
import System.IO.Unsafe
import Data.Time.Format
import System.Locale
import Data.Maybe
import System.Time
--TY import HasKAL.Misc.Environment
import HasKAL.TimeUtils.TaiUtc

{-# NOINLINE theLeapSecondTable #-}
theLeapSecondTable :: LeapSecondTable
theLeapSecondTable = parseTAIUTCDATFile taiTable
--TY theLeapSecondTable = parseTAIUTCDATFile $ unsafePerformIO $
--TY   readFile $ haskalOpt ++ "/timeTable/tai-utc.dat"
--  readFile "./HasKAL/TimeUtils/tai-utc.dat"
--TY  readFile "./tai-utc.dat"

utcbase :: UTCTime
utcbase = UTCTime (fromGregorian 1980 1 6) 0

taibase :: AbsoluteTime
taibase = utcToTAITime theLeapSecondTable utcbase

timegiven :: String -> UTCTime
timegiven s = fromJust $ parseTime defaultTimeLocale "%F %T %Z" s

taigiven :: String -> AbsoluteTime
taigiven s = utcToTAITime theLeapSecondTable (timegiven s)

time2gps :: String->String
time2gps s = init $ show (diffAbsoluteTime (taigiven s) taibase)

--for gps2time

gpsgiven :: Integer->AbsoluteTime
gpsgiven g = addAbsoluteTime (secondsToDiffTime g) taibase

utcgiven :: Integer->UTCTime
utcgiven g = taiToUTCTime theLeapSecondTable (gpsgiven g)

gps2time :: Integer->String
gps2time g = formatTime defaultTimeLocale "%F %T %Z" (utcgiven g)

name2zone :: String -> (UTCTime -> ZonedTime)
name2zone s
  | s == "JST" = utcToZonedTime (TimeZone 540 False s)
  | otherwise  = utcToZonedTime (TimeZone 0 False "UTC")
                 
gps2localTime :: Integer -> String -> String
gps2localTime g s = formatTime defaultTimeLocale "%F %T %Z" $ name2zone s (utcgiven g)

--for timetuple2gps

timestring :: String->String
timestring ss = show (readTime defaultTimeLocale "%Y %_m %e %k %_M %_S %Z" ss :: UTCTime)

tuple2string :: (Int, Int, Int, Int, Int, Int, String)->String
tuple2string (aa,bb,cc,dd,ee,ff,gg) = (show aa)++" "++(show bb)++" "++(show cc)++" "++(show dd)++" "++(show ee)++" "++(show ff)++" "++gg

timetuple2gps = time2gps.timestring.tuple2string

--for gps2timetuple

gps2Gregorian :: Integer->(Integer, Int, Int)
gps2Gregorian gg = toGregorian (utctDay (utcgiven gg))

gps2Hour :: Integer -> Int
gps2Hour gg = todHour $ timeToTimeOfDay (utctDayTime (utcgiven gg))

gps2Min :: Integer -> Int
gps2Min gg = todMin $ timeToTimeOfDay (utctDayTime (utcgiven gg))

gps2Sec :: Integer -> Int
gps2Sec gg = (floor (todSec $ timeToTimeOfDay (utctDayTime (utcgiven gg))))::Int

maketimetuple :: (Integer, Int, Int) -> Int -> Int -> Int ->(Int, Int, Int, Int, Int, Int, String)
maketimetuple (yy,mm,dd) h m s = ((fromInteger yy)::Int, mm, dd, h, m, s, "UTC")

gps2timetuple :: Integer -> (Int, Int, Int, Int, Int, Int, String)
gps2timetuple gg = maketimetuple (gps2Gregorian gg) (gps2Hour gg) (gps2Min gg) (gps2Sec gg) 

gps2localTimetuple :: Integer -> String -> (Int, Int, Int, Int, Int, Int, String)
gps2localTimetuple g s 
  | s == "JST" = (yr, mon, day, hrs, min, sec, s)
  | otherwise  = (yr, mon, day, hrs, min, sec, zone)
  where (yr, mon, day, hrs, min, sec, zone) = gps2timetuple g'
        g' = case s=="JST" of True  -> g + 32400
                              False -> g

--for Modified Julian Day(mjd)

mjdsample = 56733.302222::Double

mjdday :: Double->Double
mjdday tt = (fromIntegral (floor tt))::Double

mjdsecond :: Double->Integer
mjdsecond tt = round (86400.0*(tt - (mjdday tt)))

mjd2utc :: Double->UTCTime
mjd2utc tt = UTCTime (ModifiedJulianDay $ floor tt) (secondsToDiffTime $ mjdsecond tt)

mjd2string :: Double->String
mjd2string tt = formatTime defaultTimeLocale "%F %T %Z" (mjd2utc tt)

mjd2gps' = time2gps.mjd2string

-- for Unix Time
gps2unix :: Integer -> Integer
gps2unix gps = read $ init $ show $ utcTimeToPOSIXSeconds $ utcgiven gps

-- unix2gps :: POSIXTime -> String
-- unix2gps unix = show $ diffAbsoluteTime (utcToTAITime theLeapSecondTable (posixSecondsToUTCTime unix)) taibase

-- get Current time
getCurrentLocalTime :: String -> IO String
getCurrentLocalTime zone = do
  utc <- getCurrentTime
  return $ formatTime defaultTimeLocale "%F %T %Z" $ name2zone zone utc

getCurrentGps :: IO String
getCurrentGps = do
  strTime <- getCurrentLocalTime "UTC"
  return $ time2gps strTime

{-
These output is obsolute : Mar.4. 2014
*Main> :l gpsfunction.hs
*Main> time2gps "2013-01-01 00:00:00 UTC"
1041033616s
*Main> time2gps "2012-12-31 18:00:00 CST"
1041033616s

Latest output example : Mar.4. 2014
*Main> :l gpsfunction.hs
*Main> time2gps "2013-01-01 00:00:00 UTC"
"1041033616"
*Main> time2gps "2012-12-31 18:00:00 CST"
"1041033616"

input format is "integer"
New function output example : Jun.11. 2014
*Main> gps2time 1041033616
"2013-01-01 00:00:00 UTC"

Output example : Jun. 18. 2014
*HasKAL.TimeUtils.GPSfunction> let datesample = (2014, 3, 17, 16, 15, 12, "JST") :: (Int, Int, Int, Int, Int, Int, String)
*HasKAL.TimeUtils.GPSfunction> timetuple2gps datesample
"1079075728"

Output example : Jun. 24. 2014
56733.302222 is modified julian day of (2014, 3, 17, 16, 15, 12, "JST")
*HasKAL.TimeUtils.GPSfunction> mjd2gps' 56733.302222
"1079075728"

-}

