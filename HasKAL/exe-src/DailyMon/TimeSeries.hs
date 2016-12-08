
import System.Environment (getArgs)

import HasKAL.DataBaseUtils.FrameFull.Function (kagraWaveDataGetC, kagraDataFind)
import HasKAL.FrameUtils.FrameUtils (safeGetUnitY)
import HasKAL.PlotUtils.HROOT.PlotGraph
import HasKAL.SignalProcessingUtils.Resampling (downsampleWaveData)
import HasKAL.TimeUtils.GPSfunction (time2gps)
import HasKAL.WaveUtils.Data (WaveData(..))
import HasKAL.WaveUtils.Function (waveData2TimeSeries)

import qualified Data.Vector.Storable as V (length, fromList, map)

main = do
  args <- getArgs
  (year, month, day, ch) <- case length args of
                             4 -> return (args!!0, show0 2 (args!!1), show0 2 (args!!2), args!!3)
                             _ -> error "Usage: TimeSeries yyyy mm dd channel"

  {-- parameters --}
  let gps = read $ time2gps $ year++"-"++month++"-"++day++" 00:00:00 JST"
      duration = 86400 -- seconds
      dsfs = 2048
      -- for Plot
      oFile = ch++"-"++year++"-"++month++"-"++day++"_TimeSeries.png"
      title = "TimeSeries: " ++ ch
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
  let fs = samplingFrequency (head wd)
      wdDS = case (fs > dsfs) of
              True -> map (toHours . waveData2TimeSeries (gps,0) . downsampleWaveData dsfs) wd
              False-> map (toHours . waveData2TimeSeries (gps,0)) wd
  -- oPlotDateV Linear [Line] 1 (replicate (length wd) RED)
  --   (xlabel, unitBracket "amplitude" unit) 0.05 title oFile ((0,86400),(0,0)) gps $ wdDS
  oPlotV Linear [Line] 1 (replicate (length wd) RED) (xlabel, "amplitude") 0.05 title oFile ((0,0),(0,0)) $ wdDS

{-- Internal Functions --}
show0 :: Int -> String -> String
show0 digit number
  | len < digit = (concat $ replicate (digit-len) "0") ++ number
  | otherwise   = number
  where len = length number

unitBracket :: String -> String -> String
unitBracket x "" = x
unitBracket x y  = x++" ["++y++"]"

toHours w = (V.map (1/3600*) (fst w ), snd w)

