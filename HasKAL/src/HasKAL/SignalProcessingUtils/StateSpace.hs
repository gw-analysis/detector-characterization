

module HasKAL.SignalPocessingUtils.StateSpace
( tf2NthStateSpace
, barnes
) where


import Numeric.LinearAlgebra
import HasKAL.SignalProcessingUtils.Parallel


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



barnes :: ([Double], [Double])
       -> ([(Matrix Double,  Matrix Double,  Matrix Double)], Double)
barnes (num, denom) =
  let (x, y, z) = tf2cparallel (num, denom)
   in (map calcSS [(b, ar:+ai) | (b, ar:+ai) <- (zip y z), ai>0], head x)


calcSS :: (Complex Double, Complex Double)
       -> (Matrix Double,  Matrix Double,  Matrix Double)
calcSS (num, denom) =
  let alphar :+ alphai = num
      sigma :+ omega  = denom
      p = realPart (abs num) / (1.0 - realPart (abs ((sigma :+ omega)*(sigma :+ (-omega)))))
      r :+ q = num /((1:+0) - abs ((sigma :+ omega)*(sigma :+ (-omega))))
      kappa = ((q + q)/(p - q))**0.5
      ao = (2 >< 2) [sigma, kappa*omega, -omega/kappa, sigma]
      bo = (2 >< 1) [((realPart (abs num) - alphai)/(p-q))**0.5
                    , (-(realPart (abs num) + alphai)/(p+q))**0.5*sign alphar]
      co = (1 >< 2) [alphar/bo @@>(0, 0), alphar/bo @@>(1, 0)]
   in (ao, bo, co)

sign x
  | x < 0 = -1
  | x >= 0 = 1
  | otherwise = error "x should be minus or plus"


