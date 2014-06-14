
module HasKAL.SpectrumUtils.SpectrumUtils (
  gwpsd
)
where

{- For fft -}
import Numeric.GSL.Fourier
import Numeric.LinearAlgebra

{- psd method type -}
import HasKAL.SpectrumUtils.GwPsdMethod

{- for windowing -}
import HasKAL.SignalProcessingUtils.WindowType
import HasKAL.SignalProcessingUtils.WindowFunction

gwpsd :: [Double]-> Int -> Double -> [(Double, Double)]
gwpsd dat nfft fs = gwpsdCore Welch dat nfft fs

gwpsdCore :: PSDMETHOD -> [Double] -> Int -> Double -> [(Double, Double)]
gwpsdCore method dat nfft fs
  | method==Welch = gwpsdWelch dat nfft fs
  | otherwise =  error "No such method implemented. Check GwPsdMethod.hs"

gwpsdWelch :: [Double]-> Int -> Double -> [(Double, Double)]
gwpsdWelch dat nfft fs = do
  let ndat = length dat
      maxitr = floor $ fromIntegral (ndat) / fromIntegral (nfft) :: Int
      datlist = takesV (take maxitr (repeat nfft)) $ fromList dat
      fft_val = applyFFT . applytoComplex . applyTuplify2 . applyWindow Hann $ datlist
      power =  map (abs . fst . fromComplex) $ zipWith (*) fft_val (map conj fft_val)
      meanpower = scale (1/(fromIntegral maxitr)) $ foldr (+) (zeros nfft) power
      scale_psd = 1/(fromIntegral nfft * fs)
  zip (toList $ linspace nfft (0, fs)) (toList $ scale scale_psd meanpower)
  where
    applyFFT :: [Vector (Complex Double)] -> [Vector (Complex Double)]
    applyFFT = map fft
    applytoComplex :: [(Vector Double, Vector Double)] -> [Vector (Complex Double)]
    applytoComplex = map toComplex
    applyTuplify2 :: [Vector Double] -> [(Vector Double, Vector Double)]
    applyTuplify2 = map (tuplify2 (constant 0 nfft))
    applyWindow :: WindowType -> [Vector Double] -> [Vector Double]
    applyWindow windowtype
      | windowtype==Hann = map (windowed (hanning nfft))
      | otherwise = error "No such window implemented. Check WindowType.hs"

zeros :: Int -> Vector Double
zeros nzero = constant 0 nzero

tuplify2 :: Vector Double -> Vector Double -> (Vector Double, Vector Double)
tuplify2 x y = (y, x)




