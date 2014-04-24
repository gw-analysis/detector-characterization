

import Interpolation
import InterpolationType
--import Control.Monad
import StrictMapping

main :: IO ()
main = do
  let x = map (\a -> a + 0.5 * sin a) [0..9] :: [Double]
      y = map (\a -> a + cos (a * a)) [0..9] :: [Double]

  out <- forM' [0,1..9] $ \interp_x ->
    interp x y interp_x Spline
  print out

