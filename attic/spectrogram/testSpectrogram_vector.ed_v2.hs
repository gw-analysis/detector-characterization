import HasKAL.SpectrumUtils.SpectrumUtils
import System.Random
import Numeric.LinearAlgebra
import HasKAL.PlotUtils.HROOT.PlotGraph3D

main :: IO()
main = do
  let datlen = 20480 :: Int
      x = take datlen $ randomRs (-1, 1) $ mkStdGen 1 :: [Double]
      fs = 1024 :: Double
      nfft = 1024 :: Int
      noverlap = div nfft 2 :: Int
  spectrogram Linear COLZ " " ("Spectrogram") "testplotSpectrogram.png" $ gwspectrogram noverlap nfft fs x

