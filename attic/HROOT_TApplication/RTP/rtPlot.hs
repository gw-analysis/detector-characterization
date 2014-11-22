{-******************************************
  *     File Name: rtPlot.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/11/11 17:17:52
  *******************************************-}

import Module

import HasKAL.PlotUtils.HROOT.Histogram

main = do
  let ts = [0.0, 1.0..9.0]
      fs = [0.0, 2.0..9.0]
      xs = [1.0..fromIntegral ((length ts)*(length fs))]

      dats = format3dPlot ts fs $ dataSplit 10 0 xs
  
  -- rtPlot (10, minimum xs, maximum xs) xs
  rtPlot3D dats


format3dPlot :: [Double] -> [Double] -> [[Double]] -> [(Double, Double, Double)]
format3dPlot [] _ _ = []
format3dPlot _ [] _ = []
format3dPlot _ _ [] = []
format3dPlot (x:xs) ys (zs:zss) = zip3 [x,x..] ys zs ++ format3dPlot xs ys zss

dataSplit :: Int -> Int -> [a] -> [[a]]
dataSplit n m xs = map ((take n).(flip drop xs)) [0, (n-m)..(length xs)-n]
