
import Data.List (delete, elemIndex, intersect)
import Data.Maybe (catMaybes)
import System.Environment (getArgs)

import HasKAL.DetectorUtils.Detector (Detector(..))
import HasKAL.FrameUtils.Function (readFrameWaveData')
import HasKAL.PlotUtils.HROOT.PlotGraph
import HasKAL.SpectrumUtils.Function (mapSpectrum)
import HasKAL.SpectrumUtils.SpectrumUtils (gwOnesidedPSDWaveData)
import HasKAL.WaveUtils.Data (WaveData(..))
import HasKAL.WaveUtils.Function (catWaveData)

main = do
  {-- parameters --}
  args <- getArgs
  (ch, fname, oFile) <- case (length args, elemIndex "-o" args) of
                       (4, Just n) -> do
                         let arg' = delete "-o" $ delete (args!!(n+1)) args
                         return (arg'!!0, arg'!!1, args!!(n+1))
                       (2, Nothing) -> return (args!!0, args!!1, "X11")
                       (_, _) -> error "Usage: plottimeseries [-o output] channel filename"

  {-- read data --}
  mbWd <- readFrameWaveData' KAGRA ch fname
  let wd = case mbWd of
            Nothing -> error "Can't find data."
            Just x -> x

  {-- plot parameter --}
  let dtfft = "1.0"
      title = "#splitline{Spectrum: "++ch++" ("++z++")}{   ("++x++")}"
        where x = "GPS: "++(show . fst $ startGPSTime wd)++" ~ "++(show . fst $ stopGPSTime wd)
              z = "dt_{FFT}="++dtfft++"s"

  {-- main --}
  let snf = mapSpectrum sqrt $ gwOnesidedPSDWaveData (read dtfft) wd
  plotV LogXY Line 1 BLUE ("frequency [Hz]", "[/rHz]") 0.05 title oFile ((0,0),(0,0)) snf

