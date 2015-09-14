
import Data.Maybe (fromJust)

import HasKAL.FrameUtils.FrameUtils (getSamplingFrequency)
import HasKAL.DataBaseUtils.Function (kagraDataGet, kagraDataFind)
import HasKAL.SpectrumUtils.SpectrumUtils (gwpsdV, gwspectrogramV)
import HasKAL.MonitorUtils.SRMon.StudentRayleighMon
import HasKAL.PlotUtils.HROOT.PlotGraph3D


main = do
  {-- parameters --}
  let channel = "K1:"
      gps = 0
      duration = 86400 -- seconds
      -- for SRMon
      fftLength = 1    -- seconds
      srmLength = 3600 -- seconds
      timeShift = 1800 -- seconds
      freqResol = 16   -- Hz
      quantile  = 0.99 -- 0 < quantile < 1
      -- for Plot
      oFile = "out.png"
      title = "StudentRayleighMon:"
      xlabel = "time [s] from GPS: "++(show gps)

  {-- read data --}
  mbFiles <- kagraDataFind (fromIntegral gps) (fromIntegral duration) channel
  let file = case mbFiles of
              Nothing -> error "Can't find file"
              _ -> head $ fromJust mbFiles
  mbDat <- kagraDataGet gps duration channel
  mbFs <- getSamplingFrequency file channel
  let (dat, fs) = case (mbDat, mbFs) of
                   (Just a, Just b) -> (a, b)
                   (_, _) -> error "Can't read file"

  {-- main --}
  let snf = gwpsdV dat (truncate $ fftLength * fs) fs
      hf  = gwspectrogramV 0 (truncate $ fftLength * fs) fs dat
      nu = studentRayleighMonV (QUANT quantile) fs (truncate $ fftLength * fs) srmLength timeShift (truncate $ freqResol/fftLength) snf hf
  histgram2dM Linear COLZ (xlabel, "frequency [Hz]", "nu") title oFile ((0,0),(0,0)) nu

