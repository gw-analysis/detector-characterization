
import Numeric (showGFloat)
import System.Environment (getArgs)

import HasKAL.DataBaseUtils.FrameFull.Function (kagraWaveDataGetC, kagraDataFind)
import HasKAL.FrameUtils.FrameUtils (safeGetUnitY)
import HasKAL.PlotUtils.HROOT.PlotGraph3D
import HasKAL.SignalProcessingUtils.Resampling (downsampleWaveData)
import HasKAL.SpectrumUtils.Function (mapSpectrogram, catSpectrogramT0)
import HasKAL.SpectrumUtils.SpectrumUtils (gwspectrogramWaveData)
import HasKAL.TimeUtils.Function (diffGPS, deformatGPS)
import HasKAL.TimeUtils.GPSfunction (time2gps)
import HasKAL.WaveUtils.Data (WaveData(..))
import qualified Data.Vector.Storable as V

main = do
  args <- getArgs
  (year, month, day, ch) <- case length args of
                             4 -> return (args!!0, show0 2 (args!!1), show0 2 (args!!2), args!!3)
                             _ -> error "Usage: Spectrogram yyyy mm dd channel"

  {-- parameters --}
  let gps = read $ time2gps $ year++"-"++month++"-"++day++" 00:00:00 JST"
      duration = 86400 -- seconds
      dsfs = 2048
      -- for Spectrogram
      fftLength = 120    -- seconds
      -- for Plot
      oFile = ch++"-"++year++"-"++month++"-"++day++"_Spectrogram.png"
      title = "Spectrogram: "++ch ++" (dt= "++(show fftLength)++", df="++(showGFloat (Just 3) (1/fftLength) "")++")"
--      xlabel = "Date: "++year++"/"++month
      xlabel = "Time[hours] since "++year++"/"++month++"/"++day++" 00:00:00 JST"

  {-- read data --}
  mbWd <- kagraWaveDataGetC (fromIntegral gps) (fromIntegral duration) ch
  mbFiles <- kagraDataFind (fromIntegral gps) (fromIntegral duration) ch
  let (wd, file) = case (mbWd, mbFiles) of
              (Nothing, _) -> error $ "Can't find file: "++year++"/"++month++"/"++day
              (_, Nothing) -> error $ "Can't find file: "++year++"/"++month++"/"++day
              (Just x, Just y) -> (x, head y)
  unit <- safeGetUnitY file ch

  {-- main --}
  let hf = map (gwspectrogramWaveData 0 fftLength . downsampleWaveData dsfs) wd
      n0 = nblocks fftLength gps duration wd
--  histgram2dDateM LogYZ COLZ (xlabel, "frequency [Hz]", "[1/rHz]") title oFile ((0,0),(0,0)) gps 
--    $ mapSpectrogram sqrt $ catSpectrogramT0 0 fftLength n0 hf
  histgram2dM LogYZ COLZ (xlabel, "frequency [Hz]", "[1/rHz]") title oFile ((0,0),(0,0)) $ mapSpectrogram sqrt $ toHours $ catSpectrogramT0 0 fftLength n0 hf

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

toHours spe = (V.map (1/3600*) t, f, m)
  where (t,f,m) = spe
