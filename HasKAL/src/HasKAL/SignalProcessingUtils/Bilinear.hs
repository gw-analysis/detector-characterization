module HasKAL.SignalProcessingUtils.Bilinear
(  denominatorCore
 , numeratorCore
)
where

import HasKAL.SignalProcessingUtils.Signatures
import Data.Complex(Complex)

denominatorCore :: GammaFunc -> DeltaFunc -> Int -> Double -> Double -> Int -> Int -> Complex Double
denominatorCore gamma _ n fs fc 1 0 = gamma fs fc n 0
denominatorCore _ delta n fs fc 1 1 = delta fs fc n 1
denominatorCore gamma delta n fs fc m k
  | k == 0 = gamma fs fc n n * denominatorCore gamma delta n fs fc (m-1) 0
  | k == m = delta fs fc n n * denominatorCore gamma delta n fs fc (m-1) (k-1)
  | k < m = gamma fs fc n n * denominatorCore gamma delta n fs fc (m-1) k
    + delta fs fc n n * denominatorCore gamma delta n fs fc (m-1) (k-1)

numeratorCore :: GammaFunc -> DeltaFunc -> Int -> Double -> Double -> Int -> Int -> Complex Double
--numeratorCore _ _ _ _ _ 1 0 = 1
--numeratorCore _ _ _ _ _ 1 1 = 1
numeratorCore alpha _ n fs fc 1 0 = alpha fs fc n 0
numeratorCore _ beta n fs fc 1 1 = beta fs fc n 1
numeratorCore alpha beta n fs fc m k
  | k == 0 = alpha fs fc n n * numeratorCore alpha beta n fs fc (m-1) 0
  | k == m  = beta fs fc n n * numeratorCore alpha beta n fs fc (m-1) (k-1)
  | k < m = alpha fs fc n n * numeratorCore alpha beta n fs fc (m-1) k
    + beta fs fc n n * numeratorCore alpha beta n fs fc (m-1) (k-1)

