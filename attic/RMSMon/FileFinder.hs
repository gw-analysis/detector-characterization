
import System.Environment (getArgs)
import Data.Maybe (fromJust, catMaybes)
--import qualified Data.Vector.Generic as DVG
import qualified Numeric.LinearAlgebra as NLA
--import qualified Data.Vector.Storable as S
--import qualified Data.Vector.Unboxed  as U


import HasKAL.TimeUtils.GPSfunction (time2gps)
import HasKAL.TimeUtils.Signature (GPSTIME, Date, LocalTime)
import HasKAL.DataBaseUtils.FrameFull.Function (kagraDataGet, kagraDataFind, kagraDailyFileList)
--import HasKAL.DataBaseUtils.XEndEnv.Function (kagraDataGet, kagraDataFind, kagraDailyFileList)
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

 -- record as png file
 let oFile = year++"-"++month++"-"++day++"_FileFinder.png"
 let xlabel = "Date: "++year++"/"++month :: String
 plotDateV Linear Point 1 BLUE (xlabel, "available flag") 0.05 "" oFile ((0,0),(0.5,1.5)) gpsstart result
 return 0


{-- Internal Functions --}
dailyFileFinder :: Date -> LocalTime -> IO ((NLA.Vector Double, NLA.Vector Double))
dailyFileFinder day loc = do
 filelistmaybe <- kagraDailyFileList day loc
 case filelistmaybe of 
   Nothing -> return (NLA.fromList [], NLA.fromList [])
   Just x  -> do
     let filelist = fromJust filelistmaybe
     -- open frame file and get (GPStime[s], GPStime[ns], duration[s])
     result' <- mapM getGPSTime filelist
     let result    = catMaybes result'
     let gpsstart  = read $ time2gps $ day ++ "00:00:00 JST"
         gpsstartd = fromIntegral gpsstart :: Double
         flagvec   = NLA.fromList $ replicate (length result) 1.0 :: NLA.Vector Double
         gpssvec   = NLA.fromList $ map (+(-gpsstartd)) $ map (fromIntegral.fst') result :: NLA.Vector Double
         --    gpssvec'     = NLA.fromList $ map (fromIntegral.fst') result :: NLA.Vector Double
     return (gpssvec, flagvec)

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


 -- let flagvec     = NLA.fromList $ replicate (length result) 1.0 :: NLA.Vector Double
 --     gpssvec     = NLA.fromList $ map (+(-gpsstartd)) $ map (fromIntegral.fst') result :: NLA.Vector Double
 --     gpssvec'     = NLA.fromList $ map (fromIntegral.fst') result :: NLA.Vector Double
 --     gpsnsvec    = NLA.fromList $ map (fromIntegral.snd') result :: NLA.Vector Double
 --     durationvec = NLA.fromList $ map (thd') result :: NLA.Vector Double

-- plotDateV Linear Point 1 BLUE (xlabel, "available flag") 0.05 "" oFile ((gpsstartd,gpsendd),(0.5,1.5))\
--    0 (gpssvec', flagvec)
-- plotDateV Linear Point 1 BLUE (xlabel, "available flag") 0.05 "" oFile ((gpsstartd,gpsendd),(0.5,1.5)) gpsstart (gpssvec, flagvec)
