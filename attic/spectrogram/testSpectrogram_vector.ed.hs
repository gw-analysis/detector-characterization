import HasKAL.SpectrumUtils.SpectrumUtils
import System.Random
import Numeric.LinearAlgebra
import HasKAL.PlotUtils.HROOT.PlotGraph3D

main :: IO()
main = do
  let datlen = 20480 :: Int
      x = fromList (take datlen $ randomRs (-1, 1) $ mkStdGen 1 :: [Double])
      fs = 1024 :: Double
      nfft = 1024 :: Int
      noverlap = div nfft 2 :: Int
      nt = truncate $ (fromIntegral (datlen-nfft)) /(fromIntegral noverlap)
      spec = map (\m -> map snd $ gwpsd (toList $ subVector (m*noverlap) nfft x) nfft fs) [0..(nt-1)] :: [[Double]]
      freqV = toList $ linspace nfft (0, fs/2)
      tV = [1/fs*(fromIntegral nfft)/2*fromIntegral y|y <- [1..nt]]
      specdata = genTFData tV freqV spec
  print $ length tV
  print $ length freqV
  print $ length (spec!!1)
  print $ length specdata
  spectrogram Linear COLZ " " ("Spectrogram") "testplotSpectrogram.png" specdata

genTFData :: [Double] -> [Double] -> [[Double]] -> [(Double,   Double,   Double)]
genTFData tV freqV spec = do
  let tV' = concat [ replicate (length freqV) x | x <- tV]
      freqV'=take (length tV * length freqV) $ cycle freqV
  zip3 tV' freqV' (concat spec)
