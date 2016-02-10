
import System.Environment (getArgs)
import qualified Numeric.LinearAlgebra as NLA

import HasKAL.TimeUtils.GPSfunction (time2gps)
import HasKAL.TimeUtils.Signature (GPSTIME, Date, LocalTime)
import HasKAL.DataBaseUtils.FrameFull.Function (dailyFileFinder)
--import HasKAL.DataBaseUtils.XEndEnv.Function (dailyFileFinder)
import HasKAL.FrameUtils.FrameUtils (getGPSTime)
import HasKAL.PlotUtils.HROOT.PlotGraph

-- memo : JST is fixd. confirm it.
-- memo : If you read the frame data recorded at XEnd, change the imported module
--              FrameFull -> XEndEnv

main = do
 args <- getArgs
 (year, month, day) <- case length args of
       3 -> return (args!!0, show0 2 (args!!1), show0 2 (args!!2))
       _ -> error "Usage: FileFinder yyyy mm dd"

 let gpsstart = read $ time2gps $ year++"-"++month++"-"++day++" 00:00:00 JST"
     duration = 86400 :: Int -- seconds(fixed)
     gpsend   = gpsstart + duration :: Int

 print gpsstart
 print gpsend

 let days = (year++"-"++month++"-"++day)::Date
 result <- dailyFileFinder days "JST"

 -- output as png file
 let oFile = year++"-"++month++"-"++day++"_FileFinder.png"
 let xlabel = "Date: "++year++"/"++month :: String
 plotDateV Linear Point 1 BLUE (xlabel, "available flag") 0.05 "" oFile ((0,86400.0),(0.5,1.5)) gpsstart result
 return 0


{-- Internal Functions --}
show0 :: Int -> String -> String
show0 digit number
  | len < digit = (concat $ replicate (digit-len) "0") ++ number
  | otherwise   = number
  where len = length number
