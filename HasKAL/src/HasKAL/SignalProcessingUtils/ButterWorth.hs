
{- test code to check butter
-- import HasKAL.SignalProcessingUtils.ButterWorth
-- import HasKAL.SignalProcessingUtils.FilterType
-- main = return $ butter 2 30000 3000 Low

output is (numerator, denominator)
in this example,
([6.3964384855588e-2, -0.127928769711176, 6.3964384855588e-2], [1.0, -1.1682606671932643, 0.42411820661561617])
-}



module HasKAL.SignalProcessingUtils.ButterWorth
( butter
)
where


import HasKAL.SignalProcessingUtils.FilterType
import HasKAL.SignalProcessingUtils.Bilinear
import Data.Complex



butter :: Int -> Double -> Double -> FilterType -> ([Double], [Double])
butter n fs fc filterType =
  let denominatorList = map (denominator fs fc n) [0..n]
      numeratorList = map (numerator fs fc n) [0..n]
  in (map realPart numeratorList, map realPart denominatorList)
  where
    denominator :: Double -> Double -> Int -> Int -> Complex Double
    denominator = denominatorCore gamma delta n
    numerator :: Double -> Double -> Int -> Int -> Complex Double
    numerator = numeratorCore alpha beta n

    gamma :: Double -> Double -> Int -> Int -> Complex Double
    gamma fs' fc' n' m' = gammaCore fs' fc' n' m' filterType
    delta :: Double -> Double -> Int -> Int -> Complex Double
    delta fs' fc' n' m' = deltaCore fs' fc' n' m' filterType
    alpha :: Double -> Double -> Int -> Int -> Complex Double
    alpha fs' fc' n' m' = alphaCore fs' fc' n' m' filterType
    beta :: Double -> Double -> Int -> Int -> Complex Double
    beta fs' fc' n' m' = betaCore fs' fc' n' m' filterType



{-- Internal Funtion --}
deltaCore :: Double -> Double -> Int -> Int -> FilterType -> Complex Double
deltaCore fs fc n m filt
  | filt==Low = (-2.0 - filterPole n m * realToFrac (2.0*pi*fc/fs))
    / (2.0 - filterPole n m * realToFrac (2.0*pi*fc/fs))
  | filt==High = realToFrac (2.0*pi*fc/fs) - 2.0*filterPole n m
    / (realToFrac (2.0*pi*fc/fs) + 2.0*filterPole n m)

gammaCore :: Double -> Double -> Int -> Int -> FilterType -> Complex Double
gammaCore _ _ _ _ filt
  | filt==Low = 1.0
  | filt==High = 1.0

alphaCore :: Double -> Double -> Int -> Int -> FilterType -> Complex Double
alphaCore fs fc n m filt
  | filt==Low = foldl (\acc m' -> acc * (realToFrac (2*pi*fc/fs)
    / (2.0 - filterPole n m' * realToFrac (2.0*pi*fc/fs)))) 1.0 [1..m]
  | filt==High = 2.0 * foldl (\acc m' -> acc * (1.0 / (realToFrac (2.0*pi*fc/fs) + 2.0*filterPole n m'))) 1.0 [1..m]


betaCore :: Double -> Double -> Int -> Int -> FilterType -> Complex Double
betaCore fs fc n m filt
  | filt==Low = foldl (\acc m' -> acc * (realToFrac (2*pi*fc/fs)
    / (2.0 - filterPole n m' * realToFrac (2.0*pi*fc/fs)))) 1.0 [1..m]
  | filt==High = (-2.0) * foldl (\acc m' -> acc * (1.0 / (realToFrac (2.0*pi*fc/fs) + 2.0*filterPole n m'))) 1.0 [1..m]


filterPole :: Int -> Int -> Complex Double
filterPole n' k' = realToFrac (cos (pi*(2*k+n-1)/(2*n))) + jj * realToFrac (sin (pi*(2*k+n-1)/(2*n)))
  where n = fromIntegral n' :: Double
        k = fromIntegral k' :: Double

jj :: RealFloat a => Complex a
jj = 0 :+ 1.0



