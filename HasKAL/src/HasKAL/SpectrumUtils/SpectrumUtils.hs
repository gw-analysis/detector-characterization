

module HasKAL.SpectrumUtils.SpectrumUtils (
  gwpsd
) where


{- For fft -}
import Numeric.GSL.Fourier
import Numeric.LinearAlgebra

{- For Analysis -}


gwpsd :: Vector (Complex Double) -> Int -> Double -> Vector Double
gwpsd dat nfft fs = do
  let ndat = dim dat
      maxitr = floor $ fromIntegral (ndat) / fromIntegral (nfft)
      datlist = takesV (take maxitr (repeat nfft)) $ fst $ fromComplex dat
      fft_val = map fft
              $ map toComplex
              $ map exchange
              $ map (tuplify2 (constant 0 nfft)) (map (windowed (hanning nfft)) datlist)
      power =  map (abs . fst . fromComplex) $ zipWith (*) fft_val (map conj fft_val)
      meanpower = getVal $ map (* (1/(fromIntegral maxitr))) $ zipWith (+) power power
      scale_psd = 1/(fromIntegral nfft) * fs
  subVector 1 nfft $ scaleRecip scale_psd meanpower


getVal :: [a] -> a
getVal [a] = a

tuplify2 :: a -> a -> (a, a)
tuplify2 x y = (x, y)

exchange :: (a, a) -> (a, a)
exchange (x, y) = (y, x)

windowed :: Vector Double -> Vector Double -> Vector Double
windowed w x = w * x

hanning :: Int -> Vector Double
hanning = makeWindow hanning'

makeWindow :: (Double -> Double -> Double) -> Int -> Vector Double
makeWindow win m =
    let md = fromIntegral m
    in fromList $ map (win md . fromIntegral) [(0::Int) ..]



hanning' :: Double -> Double -> Double
hanning' m n = 0.5 - 0.5 * cos(2 * pi * n / m)



