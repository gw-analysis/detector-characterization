{-******************************************
  *     File Name: plotTest.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/08/02 01:41:15
  *******************************************-}

-- import qualified PlotModule as PM
import qualified PlotModuleForNewHROOT as PM

main :: IO ()
main = do
  let xs = [1.0..50]
      ys = reverse xs

  PM.plot PM.Linear PM.LinePoint ("xl", "yl") "a.png" $ zip xs ys
  PM.oPlot PM.Linear PM.LinePoint ("xl", "yl") "b.png" $ [zip xs xs, zip xs ys]
  PM.dPlot PM.Linear PM.LinePoint ("xl", "yl") "c.png" $ [zip xs xs, zip xs ys]
  PM.plotX PM.Linear PM.LinePoint ("xl", "yl") $ zip xs ys
  PM.oPlotX PM.Linear PM.LinePoint ("xl", "yl") $ [zip xs xs, zip xs ys] 
  PM.dPlotX PM.Linear PM.LinePoint ("xl", "yl") $ [zip xs xs, zip xs ys, zip ys xs]

