{-
 - unit test of whitening filter based on linear prediction error filter
 -}

import HasKAL.ExternalUtils.NumericalRecipes.Functions.hs
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
      psddat = gwpsd x 16384 16384
      (whnbNum, whnbDenom) = lpefCoeff 1500 psddat
      whitenedx = iirFilter x whnbNum whnbDenom
  HR.oplot HR.LogXY HR.Line ("frequency", "Spectrum") "PowerSpectrum" "beforePSD.eps" [x, whitenedx]

