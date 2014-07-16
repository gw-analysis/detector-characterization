module HasKAL.SignalProcessingUtils.Bilinear
(  denominatorCore
 , numeratorCore
)
where

import HasKAL.SignalProcessingUtils.Signatures
import Data.Complex(Complex)

denominatorCore :: GammaFunc -> DeltaFunc -> Double -> Double -> Int -> Int -> Complex Double
denominatorCore gamma delta fs fc 1 0 = gamma fs fc 1 1
denominatorCore gamma delta fs fc 1 1 = delta fs fc 1 1
denominatorCore gamma delta fs fc n m
  | m == 0 = (delta fs fc (n-1) 0) * (denominatorCore gamma delta fs fc (n-1) 0)
  | m == n  = (gamma fs fc n n) * (denominatorCore gamma delta fs fc n (n-1))
  | m < n = (gamma fs fc n m) * (denominatorCore gamma delta fs fc (n-1) m)
    + (delta fs fc n m) * (denominatorCore gamma delta fs fc (n-1) (m-1))
  | m > n  = 1

numeratorCore :: GammaFunc -> DeltaFunc -> Double -> Double -> Int -> Int -> Complex Double
numeratorCore alpha beta fs fc 1 0 = alpha fs fc 1 1
numeratorCore alpha beta fs fc 1 1 = beta fs fc 1 1
numeratorCore alpha beta fs fc n m
  | m == 0 = (alpha fs fc (n-1) 0) * (numeratorCore alpha beta fs fc (n-1) 0)
  | m == n  = (alpha fs fc n n) * (numeratorCore alpha beta fs fc n (n-1))
  | m < n = (alpha fs fc n m) * (numeratorCore alpha beta fs fc (n-1) m)
    + (beta fs fc n m) * (numeratorCore alpha beta fs fc (n-1) (m-1))
  | m > n = 1

