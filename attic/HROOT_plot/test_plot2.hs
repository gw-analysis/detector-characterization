import HasKAL.FrameUtils.FrameUtils
--import FrameUtils

{- For fft -}
import Numeric.GSL.Fourier
import Numeric.LinearAlgebra

import HROOT hiding (eval)


--PlotUtilesHROOT
--import HasKAL.FrameUtils.PlotUtilesHROOT
--import PlotUtilesHROOT
import PlotUtilsHROOT

main = do
     plot_volt "test-1066392016-300.gwf"
