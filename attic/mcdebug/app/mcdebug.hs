
import qualified Data.Vector.Storable as V
import HasKAL.DataBaseUtils.FrameFull.Function (kagraWaveDataGet0)
import HasKAL.SignalProcessingUtils.Filter(iir)
import HasKAL.SignalProcessingutils.FilterType(High)
import HasKAL.SignalProcessingUtils.Butterworth(butter)
import HasKAL.SignalProcessingUtils.Resampling (downsampleWaveData)
import System.Environment (getArgs)
import HasKAL.TimeUtils.GPSfunction (time2gps)
import HasKAL.WaveUtils.Function (waveData2TimeSeries)


main = do
 args <- getArgs
 (year, month, day, ch) <- case length args of
  4 -> return (head args, show0 2 (args!!1), show0 2 (args!!2), args!!3)
  _ -> error "Usage: debugmc yyyy mm dd channel"

 let gps = read $ time2gps $ year++"-"++month++"-"++day++" 00:00:00 JST"
     duration 86400
     newfs = 2048
     oFile5000 = ch++"-"++year++"-"++month++"-"++day++"_MCServo5000over.png"
     oFile10000 = ch++"-"++year++"-"++month++"-"++day++"_MCServo10000over.png"
     oFile15000 = ch++"-"++year++"-"++month++"-"++day++"_MCServo15000over.png"
     xlabel = "Time[hours] since "++year++"/"++month++"/"++day++"00:00:00 JST"
     title = ch++"over threshold each 10 minutes (%)"


 mbWd <- kagraWaveDataGet0 (fromIntegral gps) (fromIntegral duration) ch
 let chunkLen = 10*60 --seconds
     wd = case mbWd of
           Nothing -> error "no valid data."
           Just x -> x
     wdDS = map (waveData2TimeSeries (gps,0) . downsampleWaveData dsfs) wd
     ts =  map V.toList $ mkChunks (snd wdDS) (chunkLen*(floor newfs))
     surviver5000 = V.fromList $ map (\y-> (1/newfs*) . length $ [x | x<-y, (abs x) > 5000]) ts
     surviver10000 = V.fromList $ map (\y-> (1/newfs*) . length $ [x | x<-y, (abs x) > 10000]) ts
     surviver15000 = V.fromList $ map (\y-> (1/newfs*) . length $ [x | x<-y, (abs x) > 15000]) ts
     tvec = V.fromList [0,chunkLen/3600..chunkLen/3600*(length surviver-1)]
 plotV Linear Line 1 RED (xlabel, "percentage") 0.05 title oFile5000 ((0,0),(0,0)) $ (tvec, surviver5000)
 plotV Linear Line 1 RED (xlabel, "percentage") 0.05 title oFile10000 ((0,0),(0,0)) $ (tvec, surviver10000)
 plotV Linear Line 1 RED (xlabel, "percentage") 0.05 title oFile15000 ((0,0),(0,0)) $ (tvec, surviver15000)


mkChunks :: V.Vector Double -> Int -> [V.Vector Double]
mkChunks vIn n = mkChunksCore vIn n (V.length vIn `div` n)
  where
    mkChunksCore _ _ 0 = []
    mkChunksCore vIn n m = V.slice 0 n vIn :  mkChunksCore (V.drop n vIn) n (m-1)



