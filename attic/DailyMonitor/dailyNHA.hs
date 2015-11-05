
import Data.Maybe (fromJust)
import System.Environment (getArgs)
import Data.Packed.Vector (subVector)

import HasKAL.TimeUtils.GPSfunction (time2gps)
import HasKAL.FrameUtils.FrameUtils (getSamplingFrequency)
import HasKAL.DataBaseUtils.Function (kagraDataGet, kagraDataFind)
import HasKAL.SpectrumUtils.SpectrumUtils (gwOnesidedPSDV, gwspectrogramV)
import HasKAL.MonitorUtils.RayleighMon.RayleighMon
import HasKAL.PlotUtils.HROOT.PlotGraph


main = do
  args <- getArgs
  (year, month, day, ch) <- case length args of
                             4 -> return (args!!0, show0 2 (args!!1), show0 2 (args!!2), args!!3)
                             _ -> error "Usage: dailyRMon yyyy mm dd channel"
                             
  {-- parameters --}
  let gps = read $ time2gps $ year++"-"++month++"-"++day++" 00:00:00 JST"
      duration = 86400 -- seconds
      nsig = 2
      nframe = 2048
      nshift = 512
      nstart = 0
      quantiles  = [0.50, 0.95, 0.99] -- 0 < quantile < 1
      -- for Plot
      oFile0 = ch++"-"++year++"-"++month++"-"++day++"_amp_dailyNHA.png"
      oFile1 = ch++"-"++year++"-"++month++"-"++day++"_freq_dailyNHA.png"
      title = "RayleighMon(RED=0.5, BLUE=0.95, PINK=0.99): " ++ ch
      xlabel = "time [sec] at "++year++"/"++month++"/"++day

  {-- read data --}
  mbFiles <- kagraDataFind (fromIntegral gps) (fromIntegral duration) ch
  let file = case mbFiles of
              Nothing -> error $ "Can't find file: "++year++"/"++month++"/"++day
              _ -> head $ fromJust mbFiles
  mbDat <- kagraDataGet gps duration ch
  mbFs <- getSamplingFrequency file ch
  let (dat, fs) = case (mbDat, mbFs) of
                   (Just a, Just b) -> (a, b)
                   (Nothing, _) -> error $ "Can't read data: "++ch++"-"++year++"/"++month++"/"++day
                   (_, Nothing) -> error $ "Can't read sampling frequency: "++ch++"-"++year++"/"++month++"/"++day

  {-- main --}
  let nend = fs*duration
      result = formatNHA $ nha dat fs nsig nframe nshift nstart nend
  oPlotV Linear LinePoint 1 [RED, BLUE]
    (xlabel, "normalized noise Lv.") 0.05 title oFile0 ((0,0),(0,10)) (result!!0)
  oPlotV Linear LinePoint 1 [RED, BLUE]
    (xlabel, "normalized noise Lv.") 0.05 title oFile1 ((0,0),(0,10)) (result!!1)


{-- Internal Functions --}
show0 :: Int -> String -> String
show0 digit number
  | len < digit = (concat $ replicate (digit-len) "0") ++ number
  | otherwise   = number
  where len = length number
