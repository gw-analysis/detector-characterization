{-
- for ghci
- ghci Levinson.hs
- -L$HOME/kagra/detector-characterization/HasKAL/src/HasKAL/ExternalUtils/NumericalRecipes/
- -ltoeplz
-}

module HasKAL.SignalProcessingUtils.Levinson
( levinson
--,
) where

import HasKAL.ExternalUtils.NumericalRecipes.Function

levinson :: [Double] -> Int -> [Double]
levinson r p = do
  let rv = take p r
      rv'= [-x | x <- tail (take (p+1) r)]
      autCorr = reverse (tail rv) ++ rv
  f2d $ 1:nrToeplz (d2f autCorr) (d2f rv') p


d2f :: [Double] -> [Float]
d2f = map realToFrac

f2d :: [Float] -> [Double]
f2d = map realToFrac
