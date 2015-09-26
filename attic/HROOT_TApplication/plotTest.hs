{-******************************************
  *     File Name: plotTest.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2015/09/26 16:22:29
  *******************************************-}

import HasKAL.PlotUtils.HROOT.PlotGraph
import HasKAL.PlotUtils.HROOT.PlotGraph3D
import Data.Vector.Storable as V
import Data.Packed.Matrix

main :: IO ()
main = do
  {-- parameter --}
  let gps = 1125846017 -- 2015/9/10 00:00:00

  {-- time series data --}
  let tv1 = fromList [0,100..86400]
      xv1 = V.map (sin.(2*pi*(1/V.maximum tv1)*)) tv1

  {-- spectrogram data --}
  let tv2 = tv1
      fv2 = fromList [1..64]
      ym2 = ((V.length fv2)><(V.length tv2)) [1..fromIntegral $ (V.length tv2)*(V.length fv2)]

  {-- plot --}
  plotDateV Linear Line 1 BLUE ("x", "y") 0.05 "t" "a1.png" ((0,0),(0,0)) gps (tv1, xv1)
  histgram2dDateM Linear COLZ ("x", "y", "z") "t" "a2.png" ((0,0),(0,0)) gps (tv2, fv2, ym2)
