module ButterWorth
(
--,
)
where


import FilterType
import Data.Complex

--butter :: Int -> Double -> FilterType -> Double
--butter n freq filterType
--  | filterType==Low = butterCore n a

butterTransferFunction :: Int -> Complex Double -> Complex Double
butterTransferFunction n' s = foldl (\acc k' -> 1/(s - pole n' k')*acc) 0 [1..n']


pole :: Int -> Int -> Complex Double
pole n' k' = realToFrac (cos (pi*(2*k+n-1)/(2*n))) + j * realToFrac (sin (pi*(2*k+n-1)/(2*n)))
  where n = fromIntegral n' :: Double
        k = fromIntegral k' :: Double


j :: RealFloat a => Complex a
j = 0 :+ 1.0


getzp :: Int -> Int -> Double -> Complex Double
getzp n k freq = (2 + realToFrac dT * pole n k) / (2 - realToFrac dT * pole n k)
  where dT = 1/freq

gamma :: Double -> Int -> Int -> Complex Double
gamma freq n k = realToFrac (2.0*freq) - pole n k

delta :: Double -> Int -> Int -> Complex Double
delta freq n k = realToFrac ((-2.0)*freq) - pole n k


