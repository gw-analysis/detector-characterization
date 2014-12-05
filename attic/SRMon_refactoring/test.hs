{-******************************************
  *     File Name: test.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/12/05 17:02:15
  *******************************************-}

import Data.Packed.Vector (subVector)

import HasKAL.FrameUtils.Function (readFrameV)
import HasKAL.PlotUtils.HROOT.PlotGraph3D
import HasKAL.SpectrumUtils.SpectrumUtils (gwpsdV, gwspectrogramV)
import HasKAL.MonitorUtils.SRMon.StudentRayleighMon

main = do
  let channel = "L1:LOSC-STRAIN"
      fsample = 4096 -- [Hz]
      stride = truncate fsample -- 1 [sec]
      aveN = 128 -- average times of Sn(f)
      chunckN = 128
      shiftN = 16
      clusteringN = 16

  data1 <- readFrameV channel "/home/yamamoto/L-L1_LOSC_4_V1-842743808-4096.gwf"
  data2 <- readFrameV channel "/home/yamamoto/L-L1_LOSC_4_V1-842747904-4096.gwf"

  let snf = gwpsdV (subVector 0 (stride*aveN) data1) stride fsample -- Averaged Spectrum Sn(f)
      hfs = gwspectrogramV 0 stride fsample data2 -- Spectrogram h(t, f)
      nus = studentRayleighMonV (QUANT 0.99) fsample stride chunckN shiftN clusteringN snf hfs -- nu(t, f)

  spectrogramMX LogY COLZ "nu" "SRMon" nus

