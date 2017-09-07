
import qualified Data.Vector.Storable as V
import qualified Numeric.LinearAlgebra as NL
import HasKAL.DataBaseUtils.FrameFull.Function (kagraWaveDataGetC, kagraDataFind)
import HasKAL.FrameUtils.FrameUtils (safeGetUnitY)
import HasKAL.MonitorUtils.RangeMon.InspiralRingdownDistanceQuanta (distInspiral)
import HasKAL.PlotUtils.HROOT.PlotGraph
import HasKAL.SignalProcessingUtils.Resampling (downsampleWaveData)
import HasKAL.SpectrumUtils.Function(catSpectrogramT0)
import HasKAL.SpectrumUtils.SpectrumUtils (gwspectrogramWaveData)
import HasKAL.TimeUtils.GPSfunction (time2gps)
import HasKAL.TimeUtils.Function (deformatGPS,diffGPS)
import HasKAL.WaveUtils.Data (WaveData(..))
import System.Environment (getArgs)

main = do
 args <- getArgs
 (year, month, day, hour, minute, second, chunklen', duration', ch) <- case length args of
  9 -> return (head args, show0 2 (args!!1), show0 2 (args!!2), show0 2 (args!!3), show0 2 (args!!4), show0 2 (args!!5), args!!6, args!!7, args!!8)
  _ -> error "Usage: runRangeMonBHBH yyyy mm dd hh mm ss fftsec[s] duration[s] channel"

 let chunkLen = read chunklen' ::Int -- seconds typically 15minutes
     gps = read (time2gps $ year++"-"++month++"-"++day++" "++hour++":"++minute++":"++second++" JST") :: Int
     duration = read duration' ::Int-- seconds
     dsfs = 4096
     fftLength = fromIntegral chunkLen
     oFile = ch++"-"++year++"-"++month++"-"++day++":"++hour++":"++minute++":"++second++"JST"++"_RangeMonBHBH.png"
     dFile = ch++"-"++year++"-"++month++"-"++day++":"++hour++":"++minute++":"++second++"JST"++"_RangeMonBHBH.dat"
     xlabel = "Time[s] since "++year++"/"++month++"/"++day++":"++hour++":"++minute++":"++second++"JST"
     title = "30Mo-30Mo Inspiral Range"

 mbWd <- kagraWaveDataGetC (fromIntegral gps) (fromIntegral duration) ch
 mbFiles <- kagraDataFind (fromIntegral gps) (fromIntegral duration) ch
 let (wd, file) = case (mbWd, mbFiles) of
                   (Nothing, _) -> error $ "Can't find file: "++year++"/"++month++"/"++day
                   (_, Nothing) -> error $ "Can't find file: "++year++"/"++month++"/"++day
                   (Just x, Just y) -> (x, head y)
 unit <- safeGetUnitY file ch

 let hf = map (gwspectrogramWaveData 0 fftLength . downsampleWaveData dsfs) wd
     n0 = nblocks fftLength gps duration wd
     (vecT,vecF,specgram) = catSpectrogramT0 0 fftLength n0 hf
     x = map (\x-> zip (V.toList vecF) x) (map (V.toList) $ (NL.toColumns specgram))
     ir' = map ((0.44/(sqrt 2) *) . distInspiral 30 30) x
     ir  = V.fromList $ map infinityTo0 ir'
     vecT_hr = V.map (1*) vecT
 plotV Linear Line 1 RED (xlabel, "Inspiral Range") 0.05 title oFile ((0,0),(0,0)) $ (vecT_hr, ir)


infinityTo0 x | isInfinite x == True = 0 :: Double
              | otherwise = x :: Double


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
