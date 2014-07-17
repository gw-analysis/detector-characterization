
module Levinson
( levinson
--,
) where

import Numeric.LinearAlgebra
--import Numeric.GSL.Fourier
import NumericalRecipes.Functions

levinson :: [Double] -> Int -> [Double]
levinson r p = do
  let rv = p |> r :: Vector Double
      partialrv = subVector 1 p rv
      autCorr = toList $ join [reverseVCD partialrv, rv]
      autCorrFloat = map realToFrac autCorr :: [Float]
      rvFloat = map realToFrac (toList rv) :: [Float]
      outDouble = nr_toeplz autCorrFloat rvFloat p
  map realToFrac outDouble


reverseVCD :: Vector Double -> Vector Double
reverseVCD = fromList . reverse . toList






