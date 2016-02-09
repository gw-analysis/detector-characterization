
{-- This program needs conffile and filelist. --}{--

-- conffile format
channel: X1:HOGE-Z  # channel name
dtfft: 1.0          # data length of FFT in seconds
df: 16.0            # frequency resolution of quantile Q(p; f) in Hertz
pval: 0.99 0.5      # list of dimensionless p-Value (0 <= p <= 1)
-------------------

-- filelist format
/path/to/file/a.gwf
/path/to/file/b.gwf
...
/path/to/file/z.gwf
-------------------
--}

import Data.List (delete, elemIndex)
import Data.Maybe (catMaybes)
import System.Environment (getArgs)

import HasKAL.DetectorUtils.Detector (Detector(..))
import HasKAL.FrameUtils.Function (readFrameWaveData')
import HasKAL.Misc.ConfFile (readFileList, readConfFile)
import HasKAL.MonitorUtils.RayleighMon.RayleighMon (rayleighMonWaveData)
import HasKAL.PlotUtils.HROOT.PlotGraph
import HasKAL.WaveUtils.Data (WaveData(..))
import HasKAL.WaveUtils.Function (catWaveData)

main = do
  {-- arg check --}
  args <- getArgs 
  (conf, lst, oFile) <- case (length args, elemIndex "-o" args) of
                         (4, Just n) -> do 
                           let arg' = delete "-o" $ delete (args!!(n+1)) args
                           return (arg'!!0, arg'!!1, args!!(n+1))
                         (2, Nothing) -> return (args!!0, args!!1, "X11")
                         (_, _) -> error "Usage: studentRayleighMon [-o output] conffile filelist"
                 
  {-- read param --}
  filelist <- readFileList lst
  ([ch, dtfft, df], [qs]) <- readConfFile conf ["channel", "dtfft", "df"] ["pval"]

  {-- read data --}
  mbWd <- mapM (readFrameWaveData' KAGRA ch) filelist
  let wd = case catMaybes mbWd of
            [] -> error "Can't find file."
            xs -> catWaveData xs
  
  {-- plot parameter --}
  let clist = [RED, BLUE, PINK, GREEN, CYAN, YELLOW, BLACK]
  let title = "#splitline{RayleighMon: "++ch++"  ("++z++")}{   ("++x++y++")}"
        where x = concat $ zipWith (\c q -> (show c)++"="++q++", ") clist qs
              y = " GPS: "++(show . fst $ startGPSTime wd)++" ~ "++(show . fst $ stopGPSTime wd)
              z = "dt_{FFT}="++dtfft++"s,  df="++df++"Hz"
  let lineType = concat $ replicate (length qs) [LinePoint, Line]
      colors = concatMap (replicate 2) clist

  {-- main --}
  let result = rayleighMonWaveData (map read qs) (read dtfft) (read df) wd wd
  oPlotV Linear lineType 1 colors ("frequency [Hz]", "normalized noise Lv.") 0.05 
    title oFile ((0,0),(0,0)) $ concatMap (\(x,y) -> [x,y]) result
  
