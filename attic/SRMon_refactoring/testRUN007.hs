{-******************************************
  *     File Name: test.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2015/02/20 15:52:47
  *******************************************-}

import Data.Packed.Vector (subVector, vjoin, mapVector, fromList, dim)
import Control.Monad (liftM, zipWithM)
import HasKAL.FrameUtils.Function (readFrameV)
import HasKAL.SpectrumUtils.SpectrumUtils (gwpsdV, gwspectrogramV, lengthTime, getSpectrum)
import HasKAL.MonitorUtils.SRMon.StudentRayleighMon (FitMethod(QUANT), studentRayleighMonV)
import HasKAL.PlotUtils.HROOT.PlotGraph3D (LogOption(LogY), PlotTypeOption3D(COLZ), spectrogramM)
import ReadFiles (run007files)
import HasKAL.PlotUtils.HROOT.PlotGraph

main = do
  let channel = "fastAdc0"
      fsample = 16384 -- [Hz]
      stride = truncate fsample `div` 2 -- 0.5 [sec]
      aveN = 512 -- 256 [sec]
      chunckN = 256 -- 128 [sec]
      shiftN = 32 -- 16 [sec]
      clusteringN = 8 -- 16 [Hz]
      datForAve = take 64 $ run007files
      datForAna = drop 64 $ run007files


{-- Student-Rayleigh Monitor --}
  data1 <- liftM (mapVector (*1550e-9).vjoin) $ mapM (readFrameV channel) datForAve
  data2 <- liftM (mapVector (*1550e-9).vjoin) $ mapM (readFrameV channel) datForAna
  let snf = gwpsdV (subVector 0 (stride*aveN) data1) stride fsample -- Averaged Spectrum Sn(f)
      hfs = gwspectrogramV 0 stride fsample data2 -- Spectrogram h(t, f)
      nus = studentRayleighMonV (QUANT 0.99) fsample stride chunckN shiftN clusteringN snf hfs -- nu(t, f)
  spectrogramM LogY COLZ "nu" "SRMon" ("./RUN007_"++channel++".png") nus


