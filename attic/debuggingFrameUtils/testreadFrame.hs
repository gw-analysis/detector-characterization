{-
- Unit test for the module FrameUtils
- -}

--module UnitTestFrameUtils
--(
-- unitTestReadFrame
--)
--where

import HasKAL.FrameUtils.FrameUtils
import HasKAL.SpectrumUtils.GwPsdMethod
import HasKAL.SpectrumUtils.SpectrumUtils
import HasKAL.PlotUtils.PlotOption.PlotOptionHROOT
import qualified HasKAL.PlotUtils.HROOT.PlotGraph as HR
import Data.List

--unitTestReadFrame :: IO()
main = do
  let fname = "H-H2_RDS_C03_L2-877201786-128.gwf"
  ch <- getChannelList fname
  let [(chname, _)]=ch
--  let chname = "V1:h_16384Hz"
  fdata <- readFrame chname fname
  let x = map realToFrac (eval fdata)
  print $ findIndices isNaN x
  print $ findIndices isInfinite x
  HR.plot HR.LogXY HR.Line ("frequency","Spectrum") "PowerSpectrum" "PSDPlot.eps" $ gwpsd x 16384 16384
   