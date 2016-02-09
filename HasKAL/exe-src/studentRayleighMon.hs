
{-- This program needs conffile and filelist. --}{--

-- conffile format
channel: X1:HOGE-Z  # channel name
dtfft: 1.0          # data length of FFT in seconds
dt: 128.0           # data length for estimating \nu(f) in seconds
overlap: 124.0      # data overlap in seconds
                    #   time resolution of \nu(t, f) is (dt - overlap)
df: 16.0            # frequency resolution of quantile \nu(t,f) in Hertz
pval: 0.99          # dimensionless p-Value (0 <= p <= 1)
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
import HasKAL.MonitorUtils.SRMon.StudentRayleighMon (studentRayleighMonWaveData)
import HasKAL.PlotUtils.HROOT.PlotGraph3D
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
  ([ch, q, dtfft, dt, lap, df], _) <- readConfFile conf ["channel", "pval", "dtfft", "dt", "overlap", "df"] []

  {-- read data --}
  mbWd <- mapM (readFrameWaveData' KAGRA ch) filelist
  let wd = case catMaybes mbWd of
            [] -> error "Can't find data"
            xs -> catWaveData xs
  
  {-- plot parameter --}
  let title = "SRMon: "++ch++" ("++x++")"
        where x = "p="++q++",  dt_{FFT}="++dtfft++"s,  dt= "++dt++"s,  df= "++df++"Hz"
      xlabel = "time since GPS="++(show . fst . startGPSTime $ wd)++" [s]"

  {-- main --}
  let result = studentRayleighMonWaveData (read q) (read dtfft) (read dt) (read dt - read lap) (read df) wd wd
  histgram2dM Linear COLZ ("time [s]", "frequency [Hz]", "nu") title oFile ((0,0),(0,0)) $ result
  
