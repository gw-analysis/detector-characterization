
import Data.Maybe (fromJust)

import HasKAL.FrameUtils.FrameUtils (getSamplingFrequency)
import HasKAL.DataBaseUtils.Function (kagraDataGet, kagraDataFind)
import HasKAL.SpectrumUtils.SpectrumUtils (gwpsdV, gwspectrogramV)
import HasKAL.MonitorUtils.RayleighMon.RayleighMon
import HasKAL.PlotUtils.HROOT.PlotGraph


main = do
  {-- parameters --}
  let channel = "K1:"
      gps = 0
      duration = 86400 -- seconds
      -- for SRMon
      fftLength = 1    -- seconds
      freqResol = 16   -- Hz
      quantiles  = [0.50, 0.90, 0.95, 0.99] -- 0 < quantile < 1
      -- for Plot
      oFile = "out.png"
      title = "RayleighMon:"
      xlabel = "frequency [Hz] at GPS: "++(show gps)

  {-- read data --}
  mbFiles <- kagraDataFind (fromIntegral gps) (fromIntegral duration) channel
  let file = case mbFiles of
              Nothing -> error "Can't find file"
              _ -> head $ fromJust mbFiles
  mbDat <- kagraDataGet gps duration channel
  mbFs <- getSamplingFrequency file channel
  let (dat, fs) = case (mbDat, mbFs) of
                   (Just a, Just b) -> (a, b)
                   (Nothing, _) -> error "Can't read data"
                   (_, Nothing) -> error "Can't read sampling frequency"

  {-- main --}
  let snf = gwpsdV dat (truncate $ fftLength * fs) fs
      hf  = gwspectrogramV 0 (truncate $ fftLength * fs) fs dat
      result = rayleighMonV quantiles fs (truncate $ fftLength * fs) (truncate $ freqResol/fftLength) snf hf
  oPlotV Linear LinePoint 1 [RED, RED, BLUE, BLUE, GREEN, GREEN, PINK, PINK]
    (xlabel, "normalized noise Lv.") 0.05 title oFile ((0,0),(0,10)) $ concat $ map (\(x,y) -> [x,y]) result

