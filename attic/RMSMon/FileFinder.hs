
import System.Environment (getArgs)
import Data.Maybe (fromJust, catMaybes)
--import qualified Data.Vector.Generic as DVG
import qualified Numeric.LinearAlgebra as NLA

import HasKAL.TimeUtils.GPSfunction (time2gps)
import HasKAL.DataBaseUtils.FrameFull.Function (kagraDataGet, kagraDataFind, kagraDailyFileList)
--import HasKAL.DataBaseUtils.XEndEnv.Function (kagraDataGet, kagraDataFind, kagraDailyFileList)
import HasKAL.FrameUtils.FrameUtils (getGPSTime)
import HasKAL.PlotUtils.HROOT.PlotGraph

-- memo : JST is fixd. confirm it.
-- memo : If you read the frame data recorded at XEnd, change the imported module
--              FrameFull -> XEndEnv

main = do
 args <- getArgs
 -- (year, month, day, ch) <- case length args of
 --       4 -> return (args!!0, show0 2 (args!!1), show0 2 (args!!2), args!!3)
 --       _ -> error "Usage: FileFinder yyyy mm dd channel"

 (year, month, day) <- case length args of
       3 -> return (args!!0, show0 2 (args!!1), show0 2 (args!!2))
       _ -> error "Usage: FileFinder yyyy mm dd"

 filelistmaybe <- kagraDailyFileList (year++"-"++month++"-"++day) "JST"
 let filelist = case filelistmaybe of 
                     Nothing -> error "FileFinder : registered data is Nothing."
                     _       -> fromJust filelistmaybe

 --print filelist

 -- open frame file and get (GPStime[s], GPStime[ns], duration[s])
 result' <- mapM getGPSTime filelist
 let result = catMaybes result'
-- print result
 let flagvec     = NLA.fromList $ replicate (length result) 1.0 :: NLA.Vector Double
     gpssvec     = NLA.fromList $ map (fromIntegral.fst') result :: NLA.Vector Double
     gpsnsvec    = NLA.fromList $ map (fromIntegral.snd') result :: NLA.Vector Double
     durationvec = NLA.fromList $ map (thd') result :: NLA.Vector Double
 
 let gpsstart = read $ time2gps $ year++"-"++month++"-"++day++" 00:00:00 JST"
     duration = 86400 :: Int -- seconds(fixed)
     gpsend   = gpsstart + duration :: Int
 let gpsstartd  = fromIntegral gpsstart
     gpsendd    = fromIntegral gpsend

 print gpsstart
 print gpsend

 -- record as png file
 let oFile = year++"-"++month++"-"++day++"_FileFinder.png"
 let xlabel = "Date: "++year++"/"++month :: String
 plotDateV Linear Point 1 BLUE (xlabel, "available flag") 0.05 "" oFile ((gpsstartd,gpsendd),(0.5,1.5)) 0 (gpssvec, flagvec)

-- print $ take 10 gpslist
 return 0

{-- Internal Functions --}
-- checkFilesRegisteredOrNot :: Int -> String -> Int -> Double
-- checkFilesRegisteredOrNot tchunk channel gpsstart = do
--   filemaybe <- kagraDataFind gpsstart tchunk channel
--   let gps = case filemaybe of
--                Nothing -> -1.0
--                _       -> fromIntegral gpsstart
--   print gps

-- getGPSTimeJust :: String -- ^ file path
--                -> (Int, Int, Double) -- ^ (GPStime[s], GPStime[ns], duration[s])
-- getGPSTimeJust filelist = do
--  result <- getGPSTime filelist
--  let (gpss, gpsns, duration) = case result of
--                    (Just a, Just b, Just c) -> (a, b, c)
--                    (Nothing, _, _) -> error $ "Can't read data: "++ch++"-"++year++"/"++month++"/"++day
--                    (_, Nothing, _) -> error $ "Can't read sampling frequency: "++ch++"-"++year++"/"++month++"/"++day
--                    (_, _, Nothing) -> error $ "Can't read sampling frequency: "++ch++"-"++year++"/"++month++"/"++day

-- three element tuple
fst' :: (a, b, c) -> a
fst' (x, _, _) = x
snd' :: (a, b, c) -> b
snd' (_, y, _) = y
thd' :: (a, b, c) -> c
thd' (_, _, z) = z

show0 :: Int -> String -> String
show0 digit number
  | len < digit = (concat $ replicate (digit-len) "0") ++ number
  | otherwise   = number
  where len = length number
