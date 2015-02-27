{-******************************************
  *     File Name: plotTest.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2015/02/27 15:37:05
  *******************************************-}

import HasKAL.PlotUtils.HROOT.PlotGraph (easyPlotX, LogOption(Linear))
import HasKAL.FrameUtils.Function (readFrame)

main :: IO ()
main = do
  xs <- readFrame "L1:LOSC-STRAIN" "/data/L-L1_LOSC_4_V1-842743808-4096.gwf"
  easyPlotX Linear $ take 1000 $ zip [1..] xs
