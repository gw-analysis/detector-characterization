
import Data.Maybe (fromJust)
import System.Environment (getArgs)

import HasKAL.FrameUtils.FrameUtils (getSamplingFrequency)
import HasKAL.DataBaseUtils.Function (kagraDataGet, kagraDataFind)
import HasKAL.SpectrumUtils.SpectrumUtils (gwpsdV, gwspectrogramV)
import HasKAL.MonitorUtils.RayleighMon.RayleighMon
import HasKAL.PlotUtils.HROOT.PlotGraph


main = do
  args <- getArgs
  (ch, year, month, day) <- case length args of
                             4 -> return (args!!0, args!!1, show0 2 (args!!2), show0 2 (args!!3))
                             _ -> error "Usage: dailySRMon channel yyyy mm dd"

  {-- parameters --}
  let channel = ch
      gps = read $ time2gps $ year++"-"++month++"-"++day++" 00:00:00 JST"
      duration = 86400 -- seconds
      -- for SRMon
      fftLength = 1    -- seconds
      freqResol = 16   -- Hz
      quantiles  = [0.50, 0.90, 0.95, 0.99] -- 0 < quantile < 1
      -- for Plot
      oFile = channel++"-"++year++"-"++month++"-"++day++"_RMon.png"
      title = "RayleighMon: " ++ channel
      xlabel = "frequency [Hz] at "++year++"/"++month++"/"++day

  {-- read data --}
  mbFiles <- kagraDataFind (fromIntegral gps) (fromIntegral duration) channel
  let file = case mbFiles of
              Nothing -> error $ "Can't find file: "++year++"/"++month++"/"++day
              _ -> head $ fromJust mbFiles
  mbDat <- kagraDataGet gps duration channel
  mbFs <- getSamplingFrequency file channel
  let (dat, fs) = case (mbDat, mbFs) of
                   (Just a, Just b) -> (a, b)
                   (Nothing, _) -> error $ "Can't read data: "++ch++"-"++year++"/"++month++"/"++day
                   (_, Nothing) -> error $ "Can't read sampling frequency: "++ch++"-"++year++"/"++month++"/"++day

  {-- main --}
  let snf = gwpsdV dat (truncate $ fftLength * fs) fs
      hf  = gwspectrogramV 0 (truncate $ fftLength * fs) fs dat
      result = rayleighMonV quantiles fs (truncate $ fftLength * fs) (truncate $ freqResol/fftLength) snf hf
  oPlotV Linear LinePoint 1 [RED, RED, BLUE, BLUE, GREEN, GREEN, PINK, PINK]
    (xlabel, "normalized noise Lv.") 0.05 title oFile ((0,0),(0,10)) $ concat $ map (\(x,y) -> [x,y]) result

show0 :: Int -> String -> String
show0 digit number
  | len < digit = (concat $ replicate (digit-len) "0") ++ number
  | otherwise   = number
  where len = length number
