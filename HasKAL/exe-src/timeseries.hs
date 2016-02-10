
import Data.List (delete, elemIndex, intersect)
import Data.Maybe (catMaybes)
import System.Environment (getArgs)

import HasKAL.DetectorUtils.Detector (Detector(..))
import HasKAL.FrameUtils.Function (readFrameWaveData')
import HasKAL.Misc.ConfFile (readFileList, readConfFile)
import HasKAL.PlotUtils.HROOT.PlotGraph
import HasKAL.SignalProcessingUtils.Filter (iirW)
import HasKAL.SignalProcessingUtils.ButterWorth (FilterType(..), butter)
import HasKAL.WaveUtils.Data (WaveData(..), dropBothWaveData)
import HasKAL.WaveUtils.Function (waveData2TimeSeries, mergeOverlapWaveDataC)

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
  ([ch, lowf, highf, ndim],_) <- readConfFile conf ["channel", "lowf", "highf", "ndim"] []

  {-- read data --}
  mbWd <- mapM (readFrameWaveData' KAGRA ch) flist
  let wd = case catMaybes mbWd of
            [] -> error "Can't find data."
            xs -> mergeOverlapWaveDataC xs

  {-- plot paramter --}
  let (sGPS, eGPS) = (startGPSTime $ head wd, startGPSTime $ last wd)
      title = "Time Series: "++ch
      xlabel = "time since GPS="++(show . fst $ sGPS)++" [s]"

  {-- filter --}
  let fs = samplingFrequency $ head wd
      filt = case (read lowf, read highf, read ndim) of
              (_, _, 0) -> id
              (low, 0.0, ndim) -> dropBothWaveData (ndim*2) . iirW (butter ndim fs low High)
              (0.0, high, ndim) -> dropBothWaveData (ndim*2) . iirW (butter ndim fs high Low)
              (low, high, ndim) -> dropBothWaveData (ndim*4) . iirW (butter ndim fs low High) . iirW (butter ndim fs high Low)

  {-- main --}
  let dat = map (waveData2TimeSeries sGPS . filt) wd
  oPlotV Linear [Line] 1 (replicate (length dat) BLUE) (xlabel, "amplitude") 0.05 title oFile ((16.3,16.5),(-2e-21,2e-21)) dat

