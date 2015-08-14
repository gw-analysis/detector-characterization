

module PCA
( pca2d
, pca1d
)
where

import Numeric.LinearAlgebra

pca2d :: Matrix Double -- | row vector represents "reshape (m*n) $ toRows an image(mxn)"
      -> Int  -- | ncols of an image
      -> ([Matrix Double], Vector Double, [Matrix Double])
pca2d m l= do
  let lenm = rows m
      lenn = cols m
      (u, s, vt) = svd $ cov m
      umat = map (reshape l) $ toRows u
      vtmat = map (reshape l) $ toColumns vt
   in (umat, s, vtmat)



pca1d :: Matrix Double -> (Matrix Double, Vector Double, Matrix Double)
pca1d m = do
  let lenm = rows m
      lenn = cols m
      (u, s, vt) = svd $ cov m
   in (u, s, vt)



mean a = constant (recip . fromIntegral . rows $ a) (rows a) <> a

cov x = (trans xc <> xc) / fromIntegral (rows x - 1)
  where xc = x - asRow (mean x)


