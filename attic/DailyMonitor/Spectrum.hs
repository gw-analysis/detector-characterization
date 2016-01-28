
import qualified Data.Vector.Storable as V (length)
import Numeric (showGFloat)
import System.Environment (getArgs)

import HasKAL.DataBaseUtils.FrameFull.Function (kagraWaveDataGetC, kagraDataFind)
import HasKAL.FrameUtils.FrameUtils (safeGetUnitY)
import HasKAL.PlotUtils.HROOT.PlotGraph
import HasKAL.SpectrumUtils.SpectrumUtils (gwOnesidedPSDWaveData)
import HasKAL.SpectrumUtils.Function (mapSpectrum)
import HasKAL.TimeUtils.GPSfunction (time2gps)
import HasKAL.WaveUtils.Data (WaveData(..))
import HasKAL.WaveUtils.Function (getMaximumChunck)

main = do
  args <- getArgs
  (year, month, day, ch) <- case length args of
                             4 -> return (args!!0, show0 2 (args!!1), show0 2 (args!!2), args!!3)
                             _ -> error "Usage: Spectrum yyyy mm dd channel"

  {-- parameters --}
  let gps = read $ time2gps $ year++"-"++month++"-"++day++" 00:00:00 JST"
      duration = 86400 -- seconds
      -- for Spectrum
      fftLength = 120  -- seconds
      -- for Plot
      oFile = ch++"-"++year++"-"++month++"-"++day++"_Spectrum.png"
      xlabel = "frequency [Hz] at "++year++"/"++month++"/"++day
      title w = "#splitline{Spectrum: "++ch++" (df="++(showGFloat (Just 3) (1/fftLength) ", ")++x++")}{"++y++"}"
        where x = "AVG= "++(show $ div (V.length $ gwdata w) (truncate $ fftLength * samplingFrequency w) )
              y = "GPS: "++(show . fst $ startGPSTime w)++" ~ "++(show . fst $ stopGPSTime w)

  {-- read data --}
  mbWd <- kagraWaveDataGetC (fromIntegral gps) (fromIntegral duration) ch 
  mbFiles <- kagraDataFind (fromIntegral gps) (fromIntegral duration) ch
  let (wd, file) = case (mbWd, mbFiles) of
                    (Nothing, _) -> error $ "Can't find file: "++year++"/"++month++"/"++day
                    (_, Nothing) -> error $ "Can't find file: "++year++"/"++month++"/"++day
                    (Just x, Just y) -> (getMaximumChunck x, head y)
  unit <- safeGetUnitY file ch "" "/rHz"

  {-- main --}
  let snf = gwOnesidedPSDWaveData fftLength wd
  plotV LogXY Line 1 RED (xlabel, unit) 0.05 (title wd) oFile ((0,0),(0,0)) $ mapSpectrum sqrt snf

{-- Internal Functions --}
show0 :: Int -> String -> String
show0 digit number
  | len < digit = (concat $ replicate (digit-len) "0") ++ number
  | otherwise   = number
  where len = length number

