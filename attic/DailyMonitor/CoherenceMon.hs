
import System.Environment (getArgs)

import HasKAL.DataBaseUtils.FrameFull.Function (kagraWaveDataGetC)
import HasKAL.MonitorUtils.CoherenceMon.Function (coherenceMonW)
import HasKAL.PlotUtils.HROOT.PlotGraph
import HasKAL.TimeUtils.GPSfunction (time2gps)
import HasKAL.WaveUtils.Data (WaveData(..))
import HasKAL.WaveUtils.Function (getMaximumChunck)

main = do
  args <- getArgs
  (year, month, day, ch1, ch2) <- case length args of
                              5 -> return (args!!0, show0 2 (args!!1), show0 2 (args!!2), args!!3, args!!4)
                              _ -> error "Usage: CoherenceMon yyyy mm dd ch1 ch2"

  {-- parameters --}
  let gps = read $ time2gps $ year++"-"++month++"-"++day++" 00:00:00 JST"
      duration = 86400 -- seconds
      -- for CoherenceMon
      fftLength = 64   -- seconds
      -- for Plot
      oFile = ch1++"_"++ch2++"-"++year++"-"++month++"-"++day++"_CoherenceMon.png"
      title = "CoherenceMon: " ++ ch1 ++ " vs " ++ ch2
      xlabel = "Date: "++year++"/"++month++"/"++day

  {-- read data --}
  mbWd1 <- kagraWaveDataGetC (fromIntegral gps) (fromIntegral duration) ch1
  mbWd2 <- kagraWaveDataGetC (fromIntegral gps) (fromIntegral duration) ch2
  let (wd1, wd2) = case (mbWd1, mbWd2) of
                    (Just x, Just y) -> (getMaximumChunck x, getMaximumChunck y)
                    (Nothing, _) -> error $ "Can't read data: "++ch1++"-"++year++"/"++month++"/"++day
                    (_, Nothing) -> error $ "Can't read data: "++ch2++"-"++year++"/"++month++"/"++day

  {-- main --}
  let coh = coherenceMonW fftLength wd1 wd2
  plotV Linear Line 1 BLUE (xlabel, "|coh(f)|^2") 0.05 title oFile ((0,0),((-0.05),1.05)) coh

{-- Internal Functions --}
show0 :: Int -> String -> String
show0 digit number
  | len < digit = (concat $ replicate (digit-len) "0") ++ number
  | otherwise   = number
  where len = length number
