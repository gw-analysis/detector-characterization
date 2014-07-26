module HasKAL.SignalProcessingUtils.Bilinear
(  bilinear
)
where

import HasKAL.SignalProcessingUtils.Signatures
import Data.Complex(Complex)

bilinear :: GammaFunc -> DeltaFunc -> Int -> Double -> Double -> Int -> Int -> Complex Double
bilinear gamma _ n fs fc 1 0 = gamma fs fc n 1
bilinear _ delta n fs fc 1 1 = delta fs fc n 1
bilinear gamma delta n fs fc m k
  | k == 0 = gamma fs fc n m * bilinear gamma delta n fs fc (m-1) 0
  | k == m = delta fs fc n m * bilinear gamma delta n fs fc (m-1) (k-1)
  | k < m = gamma fs fc n m * bilinear gamma delta n fs fc (m-1) k
    + delta fs fc n m * bilinear gamma delta n fs fc (m-1) (k-1)

