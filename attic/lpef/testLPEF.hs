{-
 - unit test of whitening filter based on linear prediction error filter
 - for compilation
 - ghc -O2 --make testLPEF.hs
 - HasKAL/SignalProcessingUtils/filterFunctions.c
 - -LHasKAL/ExternalUtils/NumericalRecipes -ltoeplz -lFrame
 -}

import HasKAL.ExternalUtils.NumericalRecipes.Functions
import HasKAL.SignalProcessingUtils.Levinson
import HasKAL.SignalProcessingUtils.LinearPrediction
import HasKAL.SignalProcessingUtils.Filter
import HasKAL.SpectrumUtils.SpectrumUtils
import HasKAL.SpectrumUtils.GwPsdMethod
import HasKAL.FrameUtils.FrameUtils
import qualified HasKAL.PlotUtils.HROOT.PlotGraph as HR

main = do
  let fname = "H-H2_RDS_C03_L2-877201786-128.gwf"
  ch <- getChannelList fname
  let [(chname,  _)]=ch
  fdata <- readFrame chname fname
  let x = map realToFrac (eval fdata)
--      fre = [y/16384.0 |y<-[0.0..(realToFrac (length x) -1.0)]] :: [Double]
      psddat = gwpsd x 16384 16384
      (whnbNum, whnbDenom) = lpefCoeff 10 $ snd.unzip $ psddat
      whitenedx = firFilter x whnbDenom
  print $ take 100 whnbDenom
--  HR.oPlot HR.LogXY HR.Line ("frequency", "Spectrum") ["before", "after"] "whiteningTest.eps" [psddat, zip (fst.unzip $ psddat) whitenedx]
--  HR.plot HR.LogXY HR.Line ("frequency", "Spectrum") "after" "whiteningTest.eps" $ zip (fst.unzip $ psddat) whitenedx
  HR.plot HR.LogXY HR.Line ("frequency", "Spectrum") "after" "whiteningTest.eps" $ zip ([1..10]) whnbDenom
