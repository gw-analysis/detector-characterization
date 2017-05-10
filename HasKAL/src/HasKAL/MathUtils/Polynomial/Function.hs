module HasKAL.MathUtils.Polynomial.Function
( conv
) where

  conv :: Num a => [a] -> [a] -> [a]
  conv hs xs =
    let hs' = reverse hs
        hn = length hs
        xn = length xs
        xs' = replicate (hn-1) 0 ++ xs
    in take xn $ conv' hs' xs'

  conv' :: Num a => [a] -> [a] -> [a]
  conv' hs zs@(x:xs) = (sum $ zipWith (*) hs zs) : conv' hs xs
