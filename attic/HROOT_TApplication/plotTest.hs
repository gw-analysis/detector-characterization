{-******************************************
  *     File Name: plotTest.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/07/17 00:30:46
  *******************************************-}

import qualified PlotModule as PM

main :: IO ()
main = do
  let xs = [1.0..50]
      ys = reverse xs

  PM.plot PM.Linear PM.LinePoint ("xl", "yl") "a.png" $ zip xs ys
  PM.oPlot PM.Linear PM.LinePoint ("xl", "yl") "b.png" $ [zip xs xs, zip xs ys]
  PM.plotX PM.Linear PM.LinePoint ("xl", "yl") $ zip xs ys
  PM.oPlotX PM.Linear PM.LinePoint ("xl", "yl") $ [zip xs xs, zip xs ys] 

