module ButterWorth
( butter
--,
)
where


import FilterType
import Data.Complex


butter :: Int -> Double -> Double -> FilterType -> ([Complex Double], [Complex Double])
butter n fs fc filterType = do
  let denominatorList = map (denominator fs fc n) [0..n]
      numeratorList = map (numerator fs fc n) [0..n]
  (denominatorList, numeratorList)
  where
    denominator :: Double -> Double -> Int -> Int -> Complex Double
    denominator = denominatorCore gamma delta
    numerator :: Double -> Double -> Int -> Int -> Complex Double
    numerator = numeratorCore alpha beta

    gamma :: Double -> Double -> Int -> Int -> Complex Double
    gamma fs fc n m = gammaCore fs fc n m filterType
    delta :: Double -> Double -> Int -> Int -> Complex Double
    delta fs fc n m = deltaCore fs fc n m filterType
    alpha :: Double -> Double -> Int -> Int -> Complex Double
    alpha fs fc n m = alphaCore fs fc n m filterType
    beta :: Double -> Double -> Int -> Int -> Complex Double
    beta fs fc n m = betaCore fs fc n m filterType


{-- Internal Funtion --}

--denominator :: ( -> DeltaFunc -> Double -> Double -> Int -> Int -> Complex Double
denominatorCore gamma delta fs fc 1 0 = gamma fs fc 1 1
denominatorCore gamma delta fs fc 1 1 = delta fs fc 1 1
denominatorCore gamma delta fs fc n m
  | m < n  = (gamma fs fc n n) * (denominatorCore gamma delta fs fc n (n-1))
  | m == n = (gamma fs fc n m) * (denominatorCore gamma delta fs fc (n-1) m) + (delta fs fc n m) * (denominatorCore gamma delta fs fc (n-1) (m-1))

--numerator :: Double -> Double -> Int -> Int -> Complex Double
numeratorCore alpha beta fs fc 1 0 = alpha fs fc 1 1
numeratorCore alpha beta fs fc 1 1 = beta fs fc 1 1
numeratorCore alpha beta fs fc n m
  | m < n  = (alpha fs fc n n) * (numeratorCore alpha beta fs fc n (n-1))
  | m == n = (alpha fs fc n m) * (numeratorCore alpha beta fs fc (n-1) m) + (beta fs fc n m) * (numeratorCore alpha beta fs fc (n-1) (m-1))


deltaCore :: Double -> Double -> Int -> Int -> FilterType -> Complex Double
deltaCore fs fc n m filt
  | filt==Low = -2.0 - (filterPole n m) * realToFrac (2.0*pi*fc/fs)

gammaCore :: Double -> Double -> Int -> Int -> FilterType -> Complex Double
gammaCore fs fc n m filt
  | filt==Low = 2.0 - (filterPole n m) * realToFrac (2.0*pi*fc/fs)

alphaCore :: Double -> Double -> Int -> Int -> FilterType -> Complex Double
alphaCore fs fc n m filt
  | filt==Low = realToFrac (2*pi*fc/fs)

betaCore :: Double -> Double -> Int -> Int -> FilterType -> Complex Double
betaCore fs fc n m filt
  | filt==Low = (- realToFrac (2*pi*fc/fs))



filterPole :: Int -> Int -> Complex Double
filterPole n' k' = realToFrac (cos (pi*(2*k+n-1)/(2*n))) + jj * realToFrac (sin (pi*(2*k+n-1)/(2*n)))
  where n = fromIntegral n' :: Double
        k = fromIntegral k' :: Double

jj :: RealFloat a => Complex a
jj = 0 :+ 1.0



