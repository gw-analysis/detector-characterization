
module HasKAL.SignalProcessingUtils.Levinson
( levinson
--,
) where

import HasKAL.ExternalUtils.NumericalRecipes.Functions

levinson :: [Double] -> Int -> [Double]
levinson r p = do
  let rv = map (\m->(!!m) r) [0..p-1]
      autCorr = reverse (tail rv) ++ rv
  f2d $ 1:nrToeplz (d2f autCorr) (d2f rv) p


d2f :: [Double] -> [Float]
d2f = map realToFrac

f2d :: [Float] -> [Double]
f2d = map realToFrac
