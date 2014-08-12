{-******************************************
  *     File Name: plotTest.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/08/12 16:28:03
  *******************************************-}

import qualified HasKAL.PlotUtils.HROOT.PlotGraph as PM
import qualified HasKAL.PlotUtils.HROOT.PlotGraph3D as PM3
import qualified HasKAL.PlotUtils.HROOT.Histogram as H
import qualified HasKAL.PlotUtils.HROOT.SignalHandlerHROOT as RSH

main :: IO ()
main = do
  let xs = [1.0..50]
      ys = reverse xs

      as = [0.0..128.0]
      bs = [0.0..1024.0]
      css = mapWith (+) as bs

      ps = [-180.0..180.0]
      qs = [-90.0..90.0]
      rss = mapWith (+) ps qs

      ms = [1..10] ++ [11..17] ++ [20..22] ++ [30..37] ++ [40..49] :: [Double]
      ns = [1..6] ++ [11..15] ++ [20..21] ++ [30..34] ++ [40..46] :: [Double]
      params = (5, 0.0, 50.0) :: (Int, Double, Double) -- (nBin, xMin, xMax)

  RSH.addSignalHandle
  H.dHistX H.Linear (repeat ("X jiku", "Y jiku")) (repeat "Daimei") (repeat params) [ms, ns]
  PM.oPlotX PM.Linear PM.LinePoint ("X jiku","Y jiku") (repeat "Daimei") $ [zip xs xs, zip xs ys] 
  RSH.addSignalHandle -- 2回目呼んでも無視される
  PM.dPlotX PM.Linear PM.LinePoint (repeat ("X jiku","Y jiku")) (repeat "Daimei") $ [zip xs xs, zip xs ys, zip ys xs]
  PM3.spectrogram PM.Linear PM3.COLZ "Z jiku" ("Daimei") "X11" $ timeFreqData as bs css
  PM3.skyMap PM.Linear PM3.COLZ "Z jiku" ("Daimei") "X11" $ timeFreqData ps qs rss

timeFreqData :: [Double] -> [Double] -> [[Double]] -> [(Double, Double, Double)]
timeFreqData [] _ _ = []
timeFreqData _ [] _ = []
timeFreqData _ _ [] = []
timeFreqData (x:xs) ys (zs:zss) = zip3 [x,x..] ys zs ++ timeFreqData xs ys zss

mapWith :: (a -> b -> c) -> [a] -> [b] -> [[c]]
mapWith f [] _ = []
mapWith f (x:xs) ys = map (f x) ys : mapWith f xs ys
