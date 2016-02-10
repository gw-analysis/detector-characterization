
import Data.List (delete, elemIndex, intersect)
import Data.Maybe (catMaybes)
import System.Environment (getArgs)

import HasKAL.DetectorUtils.Detector (Detector(..))
import HasKAL.FrameUtils.Function (readFrameWaveData')
import HasKAL.Misc.ConfFile (readFileList, readConfFile)
import HasKAL.PlotUtils.HROOT.PlotGraph
import HasKAL.SignalProcessingUtils.Filter (iirW)
import HasKAL.SignalProcessingUtils.ButterWorth (FilterType(..), butter)
import HasKAL.SpectrumUtils.Function (mapSpectrum)
import HasKAL.SpectrumUtils.SpectrumUtils (gwOnesidedPSDWaveData)
import HasKAL.WaveUtils.Data (WaveData(..), dropBothWaveData)
import HasKAL.WaveUtils.Function (catWaveData)

main = do
  {-- parameters --}
  args <- getArgs
  (conf, lst, oFile) <- case (length args, elemIndex "-o" args) of
                       (4, Just n) -> do
                         let arg' = delete "-o" $ delete (args!!(n+1)) args
                         return (arg'!!0, arg'!!1, args!!(n+1))
                       (2, Nothing) -> return (args!!0, args!!1, "X11")
                       (_, _) -> error "Usage: plottimeseries [-o output] conffile filelist"

  {-- read param --}
  flist <- readFileList lst
  ([ch, dtfft, lowf, highf, ndim],_) <- readConfFile conf ["channel", "dtfft", "lowf", "highf", "ndim"] []

  {-- read data --}
  mbWd <- mapM (readFrameWaveData' KAGRA ch) flist
  let wd = case catMaybes mbWd of
            [] -> error "Can't find data."
            xs -> catWaveData xs

  {-- plot parameter --}
  let title = "#splitline{Spectrum: "++ch++" ("++z++")}{   ("++x++")}"
        where x = "GPS: "++(show . fst $ startGPSTime wd)++" ~ "++(show . fst $ stopGPSTime wd)
              z = "dt_{FFT}="++dtfft++"s"

  {-- filter --}
  let fs = samplingFrequency wd
      filt = case (read lowf, read highf, read ndim) of
              (_, _, 0) -> id
              (low, 0.0, ndim) -> dropBothWaveData (ndim*2) . iirW (butter ndim fs low High)
              (0.0, high, ndim) -> dropBothWaveData (ndim*2) . iirW (butter ndim fs high Low)
              (low, high, ndim) -> dropBothWaveData (ndim*4) . iirW (butter ndim fs low High) . iirW (butter ndim fs high Low)

  {-- main --}
  let snf = mapSpectrum sqrt $ gwOnesidedPSDWaveData (read dtfft) wd
      snf' = mapSpectrum sqrt $ gwOnesidedPSDWaveData (read dtfft) . filt $ wd
  oPlotV LogXY [Line,Line, Line] 1 [BLACK, BLUE] ("frequency [Hz]", "[/rHz]") 0.05 title oFile ((0,0),(0,0)) [snf, snf']

