import HasKAL.FrameUtils.FrameUtils
import HasKAL.SpectrumUtils.SpectrumUtils
import HasKAL.PlotUtils.PlotUtilsHROOT
import HasKAL.PlotUtils.PlotOption.PlotOptionHROOT

{- For fft -}
import Numeric.GSL.Fourier
import Numeric.LinearAlgebra

import HROOT hiding (eval)



main = do

     fdata <- readFrame "Channel_Name" "test-1066392016-300.gwf"
     let dat   = map realToFrac (eval fdata)
         lstxt = take (length dat) [1,2..]

     let lsty  = map snd $ gwpsd dat (length dat) 1000
         lstxf = take (length lsty) [1,2..]

     logLogPlot lstxf lsty "" "" Line "X11"
--     linearLinearPlot lstxf lsty "" "" Line "X11"
--     plot lstxt dat
     print ""
--     plot_sf lstxf lsty "" "" LogXY Line "test.png"
--     plot_sf lstxf lsty "" "" LogXY Line "X11"
