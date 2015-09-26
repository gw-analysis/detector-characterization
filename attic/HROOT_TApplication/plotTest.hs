{-******************************************
  *     File Name: plotTest.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2015/09/26 15:02:14
  *******************************************-}

import HasKAL.PlotUtils.HROOT.PlotGraph
import Data.Vector.Storable as V

main :: IO ()
main = do
  let gps = 1125846017 -- 2015/9/10 00:00:00
      ts = fromList [0,200..86400]

  let xs = V.map (sin.(2*pi*(1/V.maximum ts)*)) ts
  plotDateV Linear Line 1 BLUE ("x", "y") 0.05 "t" "a.png" ((0,0),(0,0)) gps (ts, xs)
