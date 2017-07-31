
import Data.List (find)
import qualified Data.Vector.Storable as V
import qualified Numeric.LinearAlgebra as NL
import HasKAL.FrameUtils.FrameUtils (safeGetUnitY)
import HasKAL.IOUtils.Function (stdin2vec)
import HasKAL.MonitorUtils.RangeMon.InspiralRingdownDistanceQuanta (distInspiral)
import HasKAL.PlotUtils.HROOT.PlotGraph
import HasKAL.SignalProcessingUtils.Resampling (downsampleWaveData)
import HasKAL.SpectrumUtils.Function(catSpectrogramT0)
import HasKAL.SpectrumUtils.SpectrumUtils (gwspectrogramWaveData)
import HasKAL.TimeUtils.GPSfunction (time2gps)
import HasKAL.TimeUtils.Function (deformatGPS,diffGPS)
import HasKAL.WaveUtils.Data (WaveData(..), vec2wave)
import System.Environment (getArgs)
import System.IO.Unsafe (unsafePerformIO)

main = do
 args <- getArgs
 (chunklen', fs', t0') <- case length args of
  3 -> return (head args, args!!1, args!!2)
  _ -> error "Usage: runRangeMonBHBHSTDIN chunklen fs t0 STDIN"

 let chunkLen = read chunklen' ::Int -- seconds typically 15minute
     fftLength = fromIntegral chunkLen
     t0 = read t0' :: Double
     fs = read fs' :: Double
     xlabel = "Time[s] since "++t0'
     title = "1.4Mo-1.4Mo Inspiral Range [pc]"
 v <- stdin2vec
 let wd = vec2wave fs t0 v

 let hf = gwspectrogramWaveData 0 fftLength wd
     (vecT,vecF,specgram) =  hf
     x = map (\x-> zip (V.toList vecF) x) (map V.toList $ (NL.toColumns specgram))
     ir'' = map ((0.44/(sqrt 2) *) . distInspiral 30 30) x
     ir'  = map infinityTo0 ir'' :: [Double]
     ir   = V.fromList ir'
     vecT_hr = V.map (1*) vecT
-- print $ V.toList ir
-- print $ V.toList vecT_hr
 plotXV Linear Line 1 RED (xlabel, "Inspiral Range[pc]") 0.05 title ((0,0),(0,0)) $ (vecT_hr, ir)
-- let xir = V.toList ir
--     xt  = V.toList vecT_hr
-- writeFile dFile $ unlines [show x1++" "++show x2|(x1,x2)<-zip xt xir]


infinityTo0 x | isInfinite x == True = 0 :: Double
              | otherwise = x :: Double
