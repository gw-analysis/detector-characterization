
import Data.List (find)
import qualified Data.Vector.Storable as V
import qualified Numeric.LinearAlgebra as NL
import HasKAL.DataBaseUtils.FrameFull.Function (kagraWaveDataGetC, kagraWaveDataGet0, kagraDataFind)
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
import System.IO.Unsafe (unsafePerformIO)

main = do
 args <- getArgs
 (year, month, day, ch) <- case length args of
  4 -> return (head args, show0 2 (args!!1), show0 2 (args!!2), args!!3)
  _ -> error "Usage: RangeMonNSNS yyyy mm dd channel"
 
 let chunkLen = 15*60 ::Int -- seconds
     gps = read $ time2gps $ year++"-"++month++"-"++day++" 00:00:00 JST"
     duration = 86400 -- seconds
     dsfs = 2048
     fftLength = fromIntegral chunkLen
     oFile = ch++"-"++year++"-"++month++"-"++day++"_RangeMonNSNS.png"
     dFile = ch++"-"++year++"-"++month++"-"++day++"_RangeMonNSNS.dat"
     xlabel = "Time[hours] since "++year++"/"++month++"/"++day++"00:00:00 JST"
     title = "1.4Mo-1.4Mo Inspiral Range [pc]"

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
     x = map (\x-> zip (V.toList vecF) x) (map (map (\x->1/9*10**(-6)*x) . V.toList) $ (NL.toColumns specgram))
     ir'' = map (10**6*) $ map ((0.44/(sqrt 2) *) . distInspiral 1.4 1.4) x
     ir'  = map infinityTo0 ir'' :: [Double]
     ir   = V.fromList $ zipWith (*) (map (\x->(checkLoking gps chunkLen x)) (V.toList vecT)) ir'
     vecT_hr = V.map (1/3600*) vecT
-- print $ V.toList ir
-- print $ V.toList vecT_hr
 plotV Linear Line 1 RED (xlabel, "Inspiral Range[pc]") 0.05 title oFile ((0,0),(0,0)) $ (vecT_hr, ir)
 let xir = V.toList ir
     xt  = V.toList vecT_hr
 writeFile dFile $ unlines [show x1++" "++show x2|(x1,x2)<-zip xt xir]


infinityTo0 x | isInfinite x == True = 0 :: Double
              | otherwise = x :: Double


checkLoking gps chunkLen t = unsafePerformIO $ do
 let t' = gps + floor t :: Int
 mbWd <- kagraWaveDataGet0 t' chunkLen "K1:GRD-MICH_LOCK_STATE_N"
 let wd = case mbWd of
           Nothing -> error "no valid file."
           Just x -> x 
     lockinfo = V.toList $ gwdata wd 
 case (find (\x-> x /= 1000) lockinfo) of
  Just _ -> return (0 :: Double)
  Nothing-> return (1 ::Double)


   
 
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


