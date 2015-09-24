

module HasKAL.MathUtils.LinearAlgebra.Function
( polyval
, toeplitz
) where



polyval :: Num a => [a] -> a -> a
polyval [] _ = 0
polyval (x:xs) x0 = x + x0 * polyval xs x0


-- | calculating Toeplitz matrix
-- | output : list of column [list]
toeplitz :: Num a => [a] -> [a] -> [[a]]
toeplitz c r =
  let nrow = length c
      ncol = length r
   in map (go c r) [0..ncol-1]
   where
     go x y i
       | i==0 = x
       | otherwise = take nx $ (reverse . take i $ drop 1 y) ++ reverse (drop i (reverse c))
       where nx = length x

