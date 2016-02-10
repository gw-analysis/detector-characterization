
import Data.List (delete, elemIndex, intersect)
import Data.Maybe (catMaybes)
import System.Environment (getArgs)

import HasKAL.DetectorUtils.Detector (Detector(..))
import HasKAL.FrameUtils.Function (readFrameWaveData')
import HasKAL.PlotUtils.HROOT.PlotGraph
import HasKAL.WaveUtils.Data (WaveData(..))
import HasKAL.WaveUtils.Function (waveData2TimeSeries, mergeOverlapWaveDataC)

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
            Just x  -> x

  {-- plot paramter --}
  let sGPS = startGPSTime wd
      eGPS = startGPSTime wd
      title = "Time Series: "++ch
      xlabel = "time since GPS="++(show . fst $ sGPS)++" [s]"

  {-- main --}
  let dat = waveData2TimeSeries (fst sGPS, 0) wd
  plotV Linear Line 1 BLUE (xlabel, "amplitude") 0.05 title oFile ((0,0),(0,0)) dat

