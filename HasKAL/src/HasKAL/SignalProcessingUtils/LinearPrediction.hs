
module HasKAL.SignalProcessingUtils.LinearPrediction
( lpefCoeff
) where

import Numeric.LinearAlgebra
import Numeric.GSL.Fourier
import Data.Complex()

{- exposed functions -}
lpefCoeff :: Int -> [Double] -> ([Double], [Double])
lpefCoeff p psddat = do
  let sqrtpsddat = map sqrt psddat
      r' = toList $ ifft $ applyRealtoComplex $ fromList sqrtpsddat
      r = take p r'
  (1:replicate p 0, levinson r p)



levinson :: [COmplex Double] -> Int -> [Double]
levinson r p = do
    let r' = array (0, nlen-1) $ zip [0..nlen-1] r
    1:(map realPart $ elems.fst $ levinson' r' p)
    where nlen = length r




{- internal functions -}
tuplify2 :: Vector Double -> Vector Double -> (Vector Double,  Vector Double)
tuplify2 x y = (y,  x)

applyRealtoComplex :: Vector Double -> Vector (Complex Double)
applyRealtoComplex x = toComplex $ tuplify2 (constant 0 nfft) x
  where nfft = dim x

levinson' :: (Ix a, Integral a, RealFloat b) => Array a (Complex b) -- ^ r
                                             -> a                   -- ^ p
                                             -> (Array a (Complex b),b) -- ^ (a,rho)
levinson' r p = (array (1,p) [ (k, a!(p,k)) | k <- [1..p] ], realPart (rho!p))
  where
    a   = array ((1,1),(p,p)) [ ((k,i), ak k i) | k <- [1..p], i <- [1..k] ]
    rho = array (1,p) [ (k, rhok k) | k <- [1..p] ]
    ak 1 1             = -r!1 / r!0
    ak k i | k==i      = -(r!k + sum [ a!(k-1,l) * r!(k-l) | l <- [1..(k-1)] ]) / rho!(k-1)
           | otherwise = a!(k-1,i) + a!(k,k) * (conjugate (a!(k-1,k-i)))
    rhok 1 = (1 - (abs (a!(1,1)))^(2::Int)) * r!0





