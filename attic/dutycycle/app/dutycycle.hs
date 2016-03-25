
import qualified Data.Vector.Storable as V
import System.Environment (getArgs)

import HasKAL.DataBaseUtils.FrameFull.Function (kagraWaveDataGetC, kagraDataFind)
import HasKAL.FrameUtils.FrameUtils (safeGetUnitY)
import HasKAL.PlotUtils.HROOT.PlotGraph
import HasKAL.TimeUtils.GPSfunction (time2gps)
import HasKAL.WaveUtils.Data (WaveData(..))
import HasKAL.WaveUtils.Function (waveData2TimeSeries)



main = do 
 args <- getArgs
 (year, month, day, hour, dur, ch) <- case length args of
  6 -> return (args!!0, show0 2 (args!!1), show0 2 (args!!2), show0 2 (args!!3), args!!4, args!!5)
  _ -> error "Usage: TimeSeries yyyy mm dd hh duration(hour) channel"

 let gps = read $ time2gps $ year++"-"++month++"-"++day++" "++hour++":00:00"++" JST"
     duration' = read dur :: Int  -- hours
     duration = fromIntegral (3600*duration') :: Double -- seconds
     oFile = ch++"-"++year++"-"++month++"-"++day++"-"++hour++"-"++dur++"_DutyCycle.png"
     xlabel = "Since "++year++"/"++month++"/"++day++"-"++"00:00:00 JST"
     hour' = read (args!!3) :: Double
 mbWd <- kagraWaveDataGetC (fromIntegral gps) (floor duration) ch
 let wd = case mbWd of
      Nothing -> error $ "Can't find file: "++year++"/"++month++"/"++day++"-"++hour++":00:00 JST"
      Just x -> x
     wdDS = map (waveData2TimeSeries (gps,0)) wd
     lockinfo = map checkLock $ V.toList $ V.concat $ (snd . unzip) wdDS
     xduty = (100/(samplingFrequency (head wd)) * (sum lockinfo))/duration
     title = "DutyCycle: "++show ((fromIntegral (truncate (xduty*100)))/100)++"%"
     x1 = map (\x->V.map (hour'+) x) $ map ((1/3600*) . fst) wdDS
     x2 = map V.fromList $ map (\x->map checkLock x) $ map (V.toList . snd) wdDS
 oPlotV Linear [Line] 1 (replicate (length wd) RED)
  (xlabel, "Lock Status") 0.05 title oFile ((0,0),(0,1.2)) $ zip x1 x2


{-- Internal Functions --}
checkLock x | x==1000 = 1 :: Double
            | otherwise = 0 ::Double

show0 :: Int -> String -> String
show0 digit number
  | len < digit = (concat $ replicate (digit-len) "0") ++ number
  | otherwise   = number
  where len = length number



