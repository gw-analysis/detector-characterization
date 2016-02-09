
import Data.List (delete, elemIndex, intersect)
import Data.Maybe (catMaybes)
import System.Environment (getArgs)

import HasKAL.DetectorUtils.Detector (Detector(..))
import HasKAL.FrameUtils.Function (readFrameWaveData')
import HasKAL.Misc.ConfFile (readFileList)
import HasKAL.PlotUtils.HROOT.PlotGraph
import HasKAL.WaveUtils.Data (WaveData(..))
import HasKAL.WaveUtils.Function (waveData2TimeSeries, mergeOverlapWaveDataC)

main = do
  {-- parameters --}
  args <- getArgs
  (ch, lst, oFile) <- case (length args, elemIndex "-o" args, elemIndex "-l" args) of
                         (5, Just n, Just m) -> do
                           let arg' = delete "-o" $ delete (args!!(n+1)) args
                               arg'' = delete "-l" $ delete (args!!(m+1)) args
                           lst <- readFileList (args!!(m+1))
                           return (head $ intersect arg' arg'', lst, args!!(n+1))
                         (4, Just n, Nothing) -> do
                           let arg' = delete "-o" $ delete (args!!(n+1)) args
                           return (arg'!!0, [arg'!!1], args!!(n+1))
                         (3, Nothing, Just m) -> do
                           let arg' = delete "-l" $ delete (args!!(m+1)) args
                           lst <- readFileList (args!!(m+1))
                           return (arg'!!0, lst, "X11")
                         (2, Nothing, Nothing) -> return (args!!0, [args!!1], "X11")
                         (_, _, _) -> error "Usage: plottimeseries [-o output] channel [filename/-l filelist]"

  {-- read data --}
  mbWd <- mapM (readFrameWaveData' KAGRA ch) lst
  let wd = case catMaybes mbWd of
            [] -> error "Can't find data."
            xs  -> mergeOverlapWaveDataC xs

  {-- plot paramter --}
  let sGPS = startGPSTime $ head wd
      eGPS = startGPSTime $ last wd
      title = "Time Series: "++ch
      xlabel = "time since GPS="++(show . fst $ sGPS)++" [s]"

  {-- main --}
  let dat = map (waveData2TimeSeries (fst sGPS, 0)) wd
  oPlotV Linear [Line] 1 (replicate (length dat) BLUE) (xlabel, "amplitude") 0.05 title oFile
    ((0,0),(0,0)) dat

