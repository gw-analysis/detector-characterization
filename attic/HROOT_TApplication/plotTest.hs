{-******************************************
  *     File Name: plotTest.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/08/06 16:26:17
  *******************************************-}

-- import qualified PlotModule as PM
import qualified PlotModuleForNewHROOT as PM
import qualified Plot3DModuleForNewHROOT as PM3

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

--   PM.plot PM.Linear PM.LinePoint ("X jiku","Y jiku") "Daimei" "a.png" $ zip xs ys
--   PM.oPlot PM.Linear PM.LinePoint (repeat ("X jiku","Y jiku")) (repeat "Daimei") "b.png" $ [zip xs xs, zip xs ys]
--   PM.dPlot PM.Linear PM.LinePoint (repeat ("X jiku","Y jiku")) (repeat "Daimei") "c.png" $ [zip xs xs, zip xs ys, zip ys xs]
--   PM3.spectrogram PM.Linear PM3.COLZ "Z jiku" ("Daimei") "d.png" dats
  PM.plotX PM.Linear PM.LinePoint ("X jiku","Y jiku") "Daimei" $ zip xs ys
  PM.oPlotX PM.Linear PM.LinePoint (repeat ("X jiku","Y jiku")) (repeat "Daimei") $ [zip xs xs, zip xs ys] 
  PM.dPlotX PM.Linear PM.LinePoint (repeat ("X jiku","Y jiku")) (repeat "Daimei") $ [zip xs xs, zip xs ys, zip ys xs]
  PM3.spectrogram PM.Linear PM3.COLZ "Z jiku" ("Daimei") "X11" $ timeFreqData as bs css
  PM3.skyMap PM.Linear PM3.AITOFF "Z jiku" ("Daimei") "X11" $ timeFreqData ps qs rss
--latitude
--longitude

timeFreqData :: [Double] -> [Double] -> [[Double]] -> [(Double, Double, Double)]
timeFreqData [] _ _ = []
timeFreqData _ [] _ = []
timeFreqData _ _ [] = []
timeFreqData (x:xs) ys (zs:zss) = zip3 [x,x..] ys zs ++ timeFreqData xs ys zss

mapWith :: (a -> b -> c) -> [a] -> [b] -> [[c]]
mapWith f [] _ = []
mapWith f (x:xs) ys = map (f x) ys : mapWith f xs ys
