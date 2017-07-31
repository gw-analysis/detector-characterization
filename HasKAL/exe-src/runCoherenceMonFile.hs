
import System.Environment (getArgs)

import HasKAL.IOUtils.Function (loadASCIIdataCV)
import HasKAL.MonitorUtils.CoherenceMon.Function (coherenceMonW)
import HasKAL.PlotUtils.HROOT.PlotGraph
import HasKAL.TimeUtils.GPSfunction (time2gps)
import HasKAL.WaveUtils.Data (WaveData(..),vec2wave)
import HasKAL.WaveUtils.Function (getMaximumChunck, getCoincidentData)

main = do
  args <- getArgs
  (fftsec', file1, t01', fs1', file2, t02', fs2') <- case length args of
    7 -> return (args!!0, args!!1, args!!2, args!!3, args!!4, args!!5, args!!6)
    _ -> error "Usage: runCoherenceMonFile fftsec file1 t01 fs1 file2 t02 fs2"

  {-- parameters --}
  let title = "CoherenceMon"
      xlabel = " time "
      fftLength = read fftsec' :: Double   -- seconds


  {-- read data --}
  let t01 = read t01' :: Double
      t02 = read t02' :: Double
      fs1 = read fs1' :: Double
      fs2 = read fs2' :: Double
      v1 = head $ loadASCIIdataCV file1
      v2 = head $ loadASCIIdataCV file2
      w1 = vec2wave fs1 t01 v1
      w2 = vec2wave fs2 t02 v2

  let [ws1, ws2] = getCoincidentData [[w1], [w2]]

  {-- main --}
  case ws1 /= [] of
   True -> do
     let [wd1, wd2] = map getMaximumChunck [ws1, ws2]
         coh = coherenceMonW fftLength wd1 wd2
     plotXV Linear Line 1 BLUE (xlabel, "|coh(f)|^2") 0.05 title ((0,0),((-0.05),1.05)) coh
   False ->
     error $ "Channels are not active at the same time.\n"
