
module HasKAL.SignalPocessingUtils.LPEF
( lpefCoeff
) where

import HasKAL.SignalProcessingUtils.Levinson
import Numeric.LinearAlgebra
import Numeric.GSL.Fourier
import Data.Complex()

{- exposed functions -}
lpefCoeff :: Int -> [Double] -> ([Double], [Double])
lpefCoeff p psddat = do
  let sqrtpsddat = map sqrt psddat
      r' = map realPart $ toList $ ifft $ applyRealtoComplex $ fromList sqrtpsddat
      r = take p r'
  (1:replicate p 0, levinson r p)


{- internal functions -}
tuplify2 :: Vector Double -> Vector Double -> (Vector Double,  Vector Double)
tuplify2 x y = (y,  x)

applyRealtoComplex :: Vector Double -> Vector (Complex Double)
applyRealtoComplex x = toComplex $ tuplify2 (constant 0 nfft) x
  where nfft = dim x

