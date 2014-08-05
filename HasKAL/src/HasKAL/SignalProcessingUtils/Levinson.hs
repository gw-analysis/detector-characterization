
module HasKAL.SignalProcessingUtils.Levinson
( levinson
--,
) where

import HasKAL.ExternalUtils.NumericalRecipes.Functions

levinson :: [Double] -> Int -> [Double]
levinson r p = do
  let rv = map (\m->(!!m) r) [0..p-1]
      autCorr = reverse (tail rv) ++ rv
      autCorrFloat = map realToFrac autCorr :: [Float]
      rvFloat = map realToFrac rv :: [Float]
      outDouble = nrToeplz autCorrFloat rvFloat p
  map realToFrac outDouble




