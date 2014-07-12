module Chebyshev
( chebyshev1
--,
)
where


import FilterType
import Data.Complex


chebyshev1:: Int -> Double -> Double -> Double -> FilterType -> ([Complex Double], [Complex Double])
chebyshev1 n r fs fc filterType = do
  let denominatorList = map (denominator fs fc n) [0..n]
      numeratorList = map (numerator fs fc n) [0..n]
  (denominatorList, numeratorList)
  where
    denominator :: Double -> Double -> Int -> Int -> Complex Double
    denominator = denominatorCore gamma delta
    numerator :: Double -> Double -> Int -> Int -> Complex Double
    numerator = numeratorCore alpha beta

    gamma :: Double -> Double -> Int -> Int -> Complex Double
    gamma fs fc n m = gammaCore r fs fc n m filterType
    delta :: Double -> Double -> Int -> Int -> Complex Double
    delta fs fc n m = deltaCore r fs fc n m filterType
    alpha :: Double -> Double -> Int -> Int -> Complex Double
    alpha fs fc n m = alphaCore r fs fc n m filterType
    beta :: Double -> Double -> Int -> Int -> Complex Double
    beta fs fc n m = betaCore r fs fc n m filterType


{-- Internal Funtion --}

denominatorCore gamma delta fs fc 1 0 = gamma fs fc 1 1
denominatorCore gamma delta fs fc 1 1 = delta fs fc 1 1
denominatorCore gamma delta fs fc n m
  | m < n  = (gamma fs fc n n) * (denominatorCore gamma delta fs fc n (n-1))
  | m == n = (gamma fs fc n m) * (denominatorCore gamma delta fs fc (n-1) m) + (delta fs fc n m) * (denominatorCore gamma delta fs fc (n-1) (m-1))

numeratorCore alpha beta fs fc 1 0 = alpha fs fc 1 1
numeratorCore alpha beta fs fc 1 1 = beta fs fc 1 1
numeratorCore alpha beta fs fc n m
  | m < n  = (alpha fs fc n n) * (numeratorCore alpha beta fs fc n (n-1))
  | m == n = (alpha fs fc n m) * (numeratorCore alpha beta fs fc (n-1) m) + (beta fs fc n m) * (numeratorCore alpha beta fs fc (n-1) (m-1))


deltaCore :: Double -> Double -> Double -> Int -> Int -> FilterType -> Complex Double
deltaCore r fs fc n m filt
  | filt==Low = -2.0 - (filterPole n m r) * realToFrac (2.0*pi*fc/fs)

gammaCore :: Double -> Double -> Double -> Int -> Int -> FilterType -> Complex Double
gammaCore r fs fc n m filt
  | filt==Low = 2.0 - (filterPole n m r) * realToFrac (2.0*pi*fc/fs)

alphaCore :: Double -> Double -> Double -> Int -> Int -> FilterType -> Complex Double
alphaCore r fs fc n m filt
  | filt==Low = (h0 n r) * realToFrac (2*pi*fc/fs)

betaCore :: Double -> Double -> Double -> Int -> Int -> FilterType -> Complex Double
betaCore r fs fc n m filt
  | filt==Low = (h0 n r) * (- realToFrac (2*pi*fc/fs))

h0 n r
  | odd n  = foldl (\acc k -> acc * filterPole n k r) 1.0 [1..n]
  | even n = realToFrac (10.0**(r/20.0)) * (foldl (\acc k -> acc * filterPole n k r) 1.0 [1..n])

filterPole :: Int -> Int -> Double -> Complex Double
filterPole n' k' r = realToFrac ((((1/gam)-gam)/2.0) * sin (pi*(2*k-1)/(2*n)))
  + jj * realToFrac ((((1/gam)+gam)/2.0) * sin (pi*(2*k-1)/(2*n)))
  where n = fromIntegral n' :: Double
        k = fromIntegral k' :: Double
        gam = ((1.0+(1.0+epcilon**2.0)**0.5)/2.0)**(1.0/n)
        epcilon = (10.0**(r/10.0)-1.0)**0.5

jj :: RealFloat a => Complex a
jj = 0 :+ 1.0



