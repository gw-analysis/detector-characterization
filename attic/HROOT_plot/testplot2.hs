import HasKAL.FrameUtils.FrameUtils
import HasKAL.SpectrumUtils.SpectrumUtils
import HasKAL.PlotUtils.PlotUtilsHROOT
--import FrameUtils

{- For fft -}
import Numeric.GSL.Fourier
import Numeric.LinearAlgebra

import HROOT hiding (eval)



main = do

     fdata <- readFrame "Channel_Name" "test-1066392016-300.gwf"
--     let dat = subVector 1 15000 (fromList $ map realToFrac (eval fdata))
     let dat   = map realToFrac (eval fdata)
         lstxt = take (length dat) [1,2..]

     let lsty  = gwpsd dat (length dat) 1000
         lstxf = take (length lsty) [1,2..]

     plot_sf lstxf lsty "" "" LogXY Line
--     plot_st lstxt dat "" "" Line




