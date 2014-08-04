{-******************************************
  *     File Name: plotTest.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/08/04 16:18:46
  *******************************************-}

-- import qualified PlotModule as PM
import qualified PlotModuleForNewHROOT as PM

main :: IO ()
main = do
  let xs = [1.0..50]
      ys = reverse xs

--   PM.plot PM.Linear PM.LinePoint ("X jiku","Y jiku") "Daimei" "a.png" $ zip xs ys
--   PM.oPlot PM.Linear PM.LinePoint (repeat ("X jiku","Y jiku")) (repeat "Daimei") "b.png" $ [zip xs xs, zip xs ys]
--   PM.dPlot PM.Linear PM.LinePoint (repeat ("X jiku","Y jiku")) (repeat "Daimei") "c.png" $ [zip xs xs, zip xs ys]
  PM.plotX PM.Linear PM.LinePoint ("X jiku","Y jiku") "Daimei" $ zip xs ys
  PM.oPlotX PM.Linear PM.LinePoint (repeat ("X jiku","Y jiku")) (repeat "Daimei") $ [zip xs xs, zip xs ys] 
  PM.dPlotX PM.Linear PM.LinePoint (repeat ("X jiku","Y jiku")) (repeat "Daimei") $ [zip xs xs, zip xs ys, zip ys xs]

