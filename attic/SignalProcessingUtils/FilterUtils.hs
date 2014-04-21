module FIlterUtils
(
  whiteningFilterFreqDomain
) where

import Numeric.LinearAlgebra
import Numeric.GSL.Fourier

whiteningFilterFreqDomain :: Vector Double -> Double -> Vector Double -> Vector Double
whiteningFilterFreqDomain psd fs dat = applyWhitening psd fs . ffted $ dat
  where
    nfft :: Int
    nfft = length $ toList dat

    ffted :: Vector Double -> Vector (Complex Double)
    ffted x = fft . toComplex . flipedtuplified . applyWindow nfft $ x

    flipedtuplified :: Vector Double -> (Vector Double, Vector Double)
    flipedtuplified = flipedtuplify2 (zeros nfft)

    applyWindow :: Int -> Vector Double -> Vector Double
    applyWindow n = windowed (hanning n)

    applyWhitening :: Vector Double -> Double -> Vector (Complex Double) -> Vector Double
    applyWhitening twosided_psd sampling_freq fftedDat =
      fst . fromComplex $ ifft $ toComplex (whitenedRealPart,  whitenedImagPart)
      where
         whitenedRealPart =
          divide (fst $ fromComplex fftedDat) (sqrt $ scale (fromIntegral nfft * sampling_freq) twosided_psd)
         whitenedImagPart = snd $ fromComplex fftedDat;

zeros :: Int -> Vector Double
zeros = constant 0

flipedtuplify2 :: Vector Double -> Vector Double -> (Vector Double, Vector Double)
flipedtuplify2 x y = (y, x)

windowed :: Vector Double -> Vector Double -> Vector Double
windowed w x = w * x

hanning :: Int -> Vector Double
hanning = makeWindow hanning'

makeWindow :: (Double -> Double -> Double) -> Int -> Vector Double
makeWindow win m =
    let md = fromIntegral m
    in fromList $ map (win md . fromIntegral) [(0::Int)..(m-1::Int)]

hanning' :: Double -> Double -> Double
hanning' m n = 0.5 - 0.5 * cos(2 * pi * n / m)


