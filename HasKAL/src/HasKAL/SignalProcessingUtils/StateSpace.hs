

module HasKAL.SignalPocessingUtils.StateSpace
( tf2NthStateSpace
) where


import Numeric.LinearAlgebra


tf2NthStateSpace :: ([Double], [Double])
                 -> (Matrix Double, Matrix Double, Matrix Double, Matrix Double)
tf2NthStateSpace (num, denom) =
  let lendenom = length denom - 1
      lennum   = length num   - 1
      maxn     = max lendenom lennum
      a | maxn > lendenom = denom ++ replicate (maxn-lendenom) (0::Double)
        | otherwise = denom
      b | maxn > lennum = num ++ replicate (maxn-lennum) (0::Double)
        | otherwise = num
      xx = toRows . fromColumns
        $ toColumns (ident (maxn-1)::Matrix Double)
        ++ [fromList (replicate (maxn-1) (0::Double))]
      aa = fromRows $ fromList (map (*(-1)) (tail a)) : xx
      bb = (maxn >< 1) ((1::Double) : replicate (maxn-1) (0::Double))
      cc = (1 >< maxn) $ zipWith (\x y -> x - head b * y) (tail b) (tail a)
      dd = (1 >< 1) [head b]
   in (aa, bb, cc, dd)




