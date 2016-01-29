
import System.Environment (getArgs)

import HasKAL.DataBaseUtils.FrameFull.Function (kagraWaveDataGetC)
import HasKAL.MonitorUtils.RayleighMon.RayleighMon (rayleighMonWaveData)
import HasKAL.PlotUtils.HROOT.PlotGraph
import HasKAL.SpectrumUtils.SpectrumUtils (gwOnesidedPSDWaveData, gwspectrogramWaveData)
import HasKAL.TimeUtils.GPSfunction (time2gps)
import HasKAL.WaveUtils.Data (WaveData(..))
import HasKAL.WaveUtils.Function (getMaximumChunck)

main = do
  args <- getArgs
  (year, month, day, ch) <- case length args of
                             4 -> return (args!!0, show0 2 (args!!1), show0 2 (args!!2), args!!3)
                             _ -> error "Usage: RMon yyyy mm dd channel"

  {-- parameters --}
  let gps = read $ time2gps $ year++"-"++month++"-"++day++" 00:00:00 JST"
      duration = 86400 -- seconds
      -- for SRMon
      fftLength = 1    -- seconds
      freqResol = 16   -- Hz
      quantiles  = [0.50, 0.95, 0.99] -- 0 < quantile < 1
      -- for Plot
      colors = [RED, BLUE, PINK]
      oFile = ch++"-"++year++"-"++month++"-"++day++"_RMon.png"
      xlabel = "frequency [Hz] at "++year++"/"++month++"/"++day
      title w = "#splitline{RayleighMon: "++ch++")}{("++x++y++")}"
        where x = concat $ zipWith (\c q -> (show c)++"="++(show q)++", ") colors quantiles
              y = " GPS: "++(show . fst $ startGPSTime w)++" ~ "++(show . fst $ stopGPSTime w)

  {-- read data --}
  mbWd <- kagraWaveDataGetC (fromIntegral gps) (fromIntegral duration) ch
  let wd = case mbWd of
            Nothing -> error $ "Can't find file: "++year++"/"++month++"/"++day
            Just x -> getMaximumChunck x

  {-- main --}
  let result = rayleighMonWaveData quantiles fftLength freqResol wd wd
  oPlotV Linear (concat $ replicate (length colors) [LinePoint, Line]) 1 (concat $ map (replicate 2) colors)
    (xlabel, "normalized noise Lv.") 0.05 (title wd) oFile ((0,0),(0,10)) $ concat $ map (\(x,y) -> [x,y]) result


{-- Internal Functions --}
show0 :: Int -> String -> String
show0 digit number
  | len < digit = (concat $ replicate (digit-len) "0") ++ number
  | otherwise   = number
  where len = length number

