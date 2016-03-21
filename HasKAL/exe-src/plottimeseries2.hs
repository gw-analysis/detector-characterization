
import System.Environment (getArgs)

import HasKAL.DataBaseUtils.FrameFull.Function (kagraWaveDataGetC, kagraDataFind)
import HasKAL.FrameUtils.FrameUtils (safeGetUnitY)
import HasKAL.PlotUtils.HROOT.PlotGraph
import HasKAL.SignalProcessingUtils.Resampling (downsampleWaveData)
import HasKAL.TimeUtils.GPSfunction (time2gps)
import HasKAL.WaveUtils.Data (WaveData(..))
import HasKAL.WaveUtils.Function (waveData2TimeSeries)

main = do
  args <- getArgs
  (year, month, day, hour, minute, second, dur, ch) <- case length args of
                             8 -> return (args!!0, show0 2 (args!!1), show0 2 (args!!2), show0 2 (args!!3), show0 2 (args!!4), show0 2 (args!!5), args!!6, args!!7)
                             _ -> error "Usage: TimeSeries yyyy mm dd hh mm ss duration channel"

  {-- parameters --}
  let gps = read $ time2gps $ year++"-"++month++"-"++day++" "++hour++":"++minute++":"++second++" JST"
      duration = read dur :: Int  -- seconds
      dsfs = 2048
      -- for Plot
      oFile = ch++"-"++year++"-"++month++"-"++day++"-"++hour++"-"++minute++"-"++second++"-"++dur++"_TimeSeries.png"
      title = ch
      xlabel = "Date: "++year++"/"++month

  {-- read data --}
  mbWd <- kagraWaveDataGetC (fromIntegral gps) (fromIntegral duration) ch
  mbFiles <- kagraDataFind (fromIntegral gps) (fromIntegral duration) ch
  let (wd, file) = case (mbWd, mbFiles) of
              (Nothing, _) -> error $ "Can't find file: "++year++"/"++month++"/"++day
              (_, Nothing) -> error $ "Can't find file: "++year++"/"++month++"/"++day
              (Just x, Just y) -> (x, head y)
  unit <- safeGetUnitY file ch

  {-- main --}
  let wdDS = map (waveData2TimeSeries (gps,0) . downsampleWaveData dsfs) wd
  oPlotDateV Linear [Line] 1 (replicate (length wd) RED)
    (xlabel, unitBracket "amplitude" unit) 0.05 title oFile ((0,0),(0,0)) gps $ wdDS


{-- Internal Functions --}
show0 :: Int -> String -> String
show0 digit number
  | len < digit = (concat $ replicate (digit-len) "0") ++ number
  | otherwise   = number
  where len = length number

unitBracket :: String -> String -> String
unitBracket x "" = x
unitBracket x y  = x++" ["++y++"]"
