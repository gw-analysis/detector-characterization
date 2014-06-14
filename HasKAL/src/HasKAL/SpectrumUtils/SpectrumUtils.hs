
module HasKAL.SpectrumUtils.SpectrumUtils (
  gwpsd
)
where

{- For fft -}
import Numeric.GSL.Fourier
import Numeric.LinearAlgebra

{- psd method type -}
import HasKAL.SpectrumUtils.GwPsdMethod


gwpsd :: [Double]-> Int -> Double -> [(Double, Double)]
gwpsd dat nfft fs = gwpsdCore Welch dat nfft fs

gwpsdCore method dat nfft fs
  | method==Welch = gwpsdWelch dat nfft fs
  | otherwise =  error "No such method inmplemnted. Check GwPsdMethod.hs"

gwpsdWelch :: [Double]-> Int -> Double -> [(Double, Double)]
gwpsdWelch dat nfft fs = do
  let ndat = length dat
      maxitr = floor $ fromIntegral (ndat) / fromIntegral (nfft) :: Int
      datlist = takesV (take maxitr (repeat nfft)) $ fromList dat
      fft_val = applyFFT . applytoComplex . applyTuplify2 . applyWindow $ datlist
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
    applyWindow :: [Vector Double] -> [Vector Double]
    applyWindow = map (windowed (hanning nfft))

zeros :: Int -> Vector Double
zeros nzero = constant 0 nzero

tuplify2 :: Vector Double -> Vector Double -> (Vector Double, Vector Double)
tuplify2 x y = (y, x)

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



