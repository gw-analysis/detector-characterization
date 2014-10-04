{-******************************************
  *     File Name: plotTest.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/10/04 21:18:08
  *******************************************-}

import qualified HasKAL.PlotUtils.HROOT.PlotGraph as PM
import qualified HasKAL.PlotUtils.HROOT.PlotGraph3D as PM3
import qualified HasKAL.PlotUtils.HROOT.Histogram as H
import qualified HasKAL.PlotUtils.HROOT.AppendFunctionHROOT as HAF

main :: IO ()
main = do
  let xs = [1.0..50]
      ys = reverse xs
      range = ((0,0),(0,20))
      ranges = [((-10,60),(-10,60)),((10,30),(10,30)),((10,50),(20,40))]

  HAF.addSignalHandle
  PM.oPlotX PM.Linear PM.LinePoint 2 ("X jiku","Y jiku") 0.05 "Daimei" range $ [zip xs xs, zip xs ys] 
  -- PM.dPlotX PM.Linear PM.LinePoint 2 (repeat ("X jiku","Y jiku")) 0.06 (repeat "Daimei") ranges $ [zip xs xs, zip xs xs, zip xs xs]

timeFreqData :: [Double] -> [Double] -> [[Double]] -> [(Double, Double, Double)]
timeFreqData [] _ _ = []
timeFreqData _ [] _ = []
timeFreqData _ _ [] = []
timeFreqData (x:xs) ys (zs:zss) = zip3 [x,x..] ys zs ++ timeFreqData xs ys zss

mapWith :: (a -> b -> c) -> [a] -> [b] -> [[c]]
mapWith f [] _ = []
mapWith f (x:xs) ys = map (f x) ys : mapWith f xs ys
