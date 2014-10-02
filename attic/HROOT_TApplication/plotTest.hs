{-******************************************
  *     File Name: plotTest.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/10/02 18:36:12
  *******************************************-}

import qualified HasKAL.PlotUtils.HROOT.PlotGraph as PM
import qualified HasKAL.PlotUtils.HROOT.PlotGraph3D as PM3
import qualified HasKAL.PlotUtils.HROOT.Histogram as H
import qualified HasKAL.PlotUtils.HROOT.SignalHandlerHROOT as RSH

main :: IO ()
main = do
  let xs = [1.0..50]
      ys = reverse xs
      range = ((0,0),(0,20))
      ranges = [((-10,60),(-10,60)),((10,30),(10,30)),((10,50),(20,40))]

  RSH.addSignalHandle
  PM.oPlotX PM.Linear PM.LinePoint ("X jiku","Y jiku") "Daimei" range $ [zip xs xs, zip xs ys] 
  PM.dPlotX PM.Linear PM.LinePoint (repeat ("X jiku","Y jiku")) (repeat "Daimei") ranges $ [zip xs xs, zip xs xs, zip xs xs]

timeFreqData :: [Double] -> [Double] -> [[Double]] -> [(Double, Double, Double)]
timeFreqData [] _ _ = []
timeFreqData _ [] _ = []
timeFreqData _ _ [] = []
timeFreqData (x:xs) ys (zs:zss) = zip3 [x,x..] ys zs ++ timeFreqData xs ys zss

mapWith :: (a -> b -> c) -> [a] -> [b] -> [[c]]
mapWith f [] _ = []
mapWith f (x:xs) ys = map (f x) ys : mapWith f xs ys
