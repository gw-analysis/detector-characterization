module HasKAL.SignalProcessingUtils.Chebyshev
( chebyshev1
)
where

import HasKAL.SignalProcessingUtils.FilterType
import HasKAL.SignalProcessingUtils.Bilinear
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

deltaCore :: Double -> Double -> Double -> Int -> Int -> FilterType -> Complex Double
deltaCore r fs fc n m filt
  | filt==Low = -2.0 - (filterPole n m r) * realToFrac (2.0*pi*fc/fs)
  | filt==High = realToFrac (2.0*pi*fc/fs) - 2.0*(filterPole n m r)

gammaCore :: Double -> Double -> Double -> Int -> Int -> FilterType -> Complex Double
gammaCore r fs fc n m filt
  | filt==Low = 2.0 - (filterPole n m r) * realToFrac (2.0*pi*fc/fs)
  | filt==High = realToFrac (2.0*pi*fc/fs) + 2.0*(filterPole n m r)

alphaCore :: Double -> Double -> Double -> Int -> Int -> FilterType -> Complex Double
alphaCore r fs fc n m filt
  | filt==Low = (h0 n r) * realToFrac (2*pi*fc/fs)
  | filt==High = 2 * h0 n r

betaCore :: Double -> Double -> Double -> Int -> Int -> FilterType -> Complex Double
betaCore r fs fc n m filt
  | filt==Low = (h0 n r) * (- realToFrac (2*pi*fc/fs))
  | filt==High = (-2) * h0 n r

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


