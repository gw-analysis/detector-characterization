
import System.Environment (getArgs)
import Data.Maybe (fromJust)

import HasKAL.TimeUtils.GPSfunction (time2gps)
import HasKAL.DataBaseUtils.XEndEnv.Function (kagraDataGet, kagraDataFind)
import HasKAL.PlotUtils.HROOT.PlotGraph

-- memo : JST is fixd. confirm it.

main = do
 args <- getArgs
 (year, month, day, ch) <- case length args of
       4 -> return (args!!0, show0 2 (args!!1), show0 2 (args!!2), args!!3)
       _ -> error "Usage: FileFinder yyyy mm dd channel"

 let gpsstart = read $ time2gps $ year++"-"++month++"-"++day++" 00:00:00 JST"
     duration = 86400 :: Int -- seconds(fixed)
     gpsend   = gpsstart + duration :: Int
     tchunk   = 32.0  -- second(fixed), 2700 files

 let gpsstartd  = fromIntegral gpsstart
     gpsendd    = fromIntegral gpsend
     gpslist    = [gpsstartd, gpsstartd + tchunk..gpsendd] :: [Double]

 -- let filesFound = map (checkFilesRegisteredOrNot) gpslist
 -- リストから負の数字のものだけを除外する filterで

 -- plotDate Linear Line 1 BLUE ("x", "y") 0.05 "t" "a1.png" ((0,0),(0,0)) gps (tv1, xv1)

 print $ take 10 gpslist
 return 0

{-- Internal Functions --}
checkFilesRegisteredOrNot :: Int -> String -> Int -> Double
checkFilesRegisteredOrNot tchunk channel gpsstart = do
  filemaybe <- kagraDataFind gpsstart tchunk channel
  let gps = case filemaybe of
               Nothing -> -1.0
               _       -> fromIntegral gpsstart
  print gps

show0 :: Int -> String -> String
show0 digit number
  | len < digit = (concat $ replicate (digit-len) "0") ++ number
  | otherwise   = number
  where len = length number
