
import qualified Data.Vector.Storable as V
import HasKAL.DataBaseUtils.FrameFull.Function (kagraWaveDataGet0)
import HasKAL.PlotUtils.HROOT.PlotGraph
import HasKAL.SignalProcessingUtils.Filter(iir)
import HasKAL.SignalProcessingUtils.FilterType(FilterType(..))
import HasKAL.SignalProcessingUtils.ButterWorth(butter)
import HasKAL.SignalProcessingUtils.Resampling (downsampleWaveData)
import System.Environment (getArgs)
import HasKAL.TimeUtils.Function (deformatGPS,diffGPS)
import HasKAL.TimeUtils.GPSfunction (time2gps)
import HasKAL.WaveUtils.Data (WaveData(..))
import HasKAL.WaveUtils.Function (waveData2TimeSeries)


main = do
 args <- getArgs
 (year, month, day, ch) <- case length args of
  4 -> return (head args, show0 2 (args!!1), show0 2 (args!!2), args!!3)
  _ -> error "Usage: debugmc yyyy mm dd channel"

 let gps = read $ time2gps $ year++"-"++month++"-"++day++" 00:00:00 JST"
     duration = 86400
     newfs = 2048
     oFile5000 = ch++"-"++year++"-"++month++"-"++day++"_MCServo5000over.png"
     oFile10000 = ch++"-"++year++"-"++month++"-"++day++"_MCServo10000over.png"
     oFile15000 = ch++"-"++year++"-"++month++"-"++day++"_MCServo15000over.png"
     xlabel = "Time[hours] since "++year++"/"++month++"/"++day++"00:00:00 JST"
     title = ch++"over threshold each 10 minutes (%)"


 mbWd <- kagraWaveDataGet0 (fromIntegral gps) (fromIntegral duration) ch
 let chunkLen = 10*60 --seconds
     chunkLenD = fromIntegral chunkLen
     wd = case mbWd of
           Nothing -> error "no valid data."
           Just x -> x
     wdDS = (waveData2TimeSeries (gps,0) . downsampleWaveData newfs) wd
     ts =  map V.toList $ mkChunks (snd wdDS) (chunkLen*(floor newfs))
     surviver5000 = V.fromList $ map (\y-> (1/newfs*) . fromIntegral . length $ [x | x<-y, (abs x) > 5000]) ts
     surviver10000 = V.fromList $ map (\y-> (1/newfs*) . fromIntegral . length $ [x | x<-y, (abs x) > 10000]) ts
     surviver15000 = V.fromList $ map (\y-> (1/newfs*) . fromIntegral . length $ [x | x<-y, (abs x) > 15000]) ts
     tvec = V.fromList ([0,chunkLenD/3600..chunkLenD/3600*(fromIntegral (V.length surviver5000)-1)]::[Double])
 plotV Linear Line 1 RED (xlabel, "percentage") 0.05 title oFile5000 ((0,0),(0,0)) $ (tvec, surviver5000)
 plotV Linear Line 1 RED (xlabel, "percentage") 0.05 title oFile10000 ((0,0),(0,0)) $ (tvec, surviver10000)
 plotV Linear Line 1 RED (xlabel, "percentage") 0.05 title oFile15000 ((0,0),(0,0)) $ (tvec, surviver15000)


mkChunks :: V.Vector Double -> Int -> [V.Vector Double]
mkChunks vIn n = mkChunksCore vIn n (V.length vIn `div` n)
  where
    mkChunksCore _ _ 0 = []
    mkChunksCore vIn n m = V.slice 0 n vIn :  mkChunksCore (V.drop n vIn) n (m-1)


{-- Internal Functions --}
show0 :: Int -> String -> String
show0 digit number
  | len < digit = (concat $ replicate (digit-len) "0") ++ number
  | otherwise   = number
  where len = length number

nblocks :: Double -> Int -> Int -> [WaveData] -> [Int]
nblocks dt gps duration [] = [ceiling . (/dt) . fromIntegral $ duration]
nblocks dt gps duration ws = map (ceiling . (/dt) . deformatGPS . uncurry diffGPS) ss
  where ss = (startGPSTime $ head ws, (gps,0))
             : map (\i -> (startGPSTime $ ws!!i, stopGPSTime $ ws !!(i-1)) ) [1..length ws -1]
             ++ [((gps+duration, 0), stopGPSTime $ last ws)]

