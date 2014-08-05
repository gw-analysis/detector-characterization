{-# LANGUAGE BangPatterns #-}

module HasKAL.SignalPocessingUtils.LPEF
( lpefCoeff
) where

import HasKAL.SignalProcessingUtils.Levinson
import HasKAL.SignalProcessingUtils.ButterWorth
import HasKAL.SignalProcessingUtils.FilterType
import HasKAL.SignalProcessingUtils.Filter
import Data.List
import Numeric.LinearAlgebra
import Numeric.GSL.Fourier
import Data.Complex




lpefCoeff :: Int -> [Double] -> ([Double], [Double])
lpefCoeff p psddat = do
  let sqrtpsddat = map sqrt psddat
      r' = map realPart $ toList $ ifft $ applyRealtoComplex $ fromList sqrtpsddat
      r = take p r'
  (1:(take (p-1) (repeat 0)), levinson r p)


-- |Numerically stable mean
-- from hstats
mean :: Floating a => [a] -> a
mean x = fst $ foldl' (\(!m,  !n) x -> (m+(x-m)/(n+1), n+1)) (0, 0) x

tuplify2 :: Vector Double -> Vector Double -> (Vector Double,  Vector Double)
tuplify2 x y = (y,  x)

applyRealtoComplex :: Vector Double -> Vector (Complex Double)
applyRealtoComplex x = toComplex $ tuplify2 (constant 0 nfft) x
  where nfft = dim x

