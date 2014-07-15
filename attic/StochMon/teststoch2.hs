import HasKAL.FrameUtils.FrameUtils
import HasKAL.SpectrumUtils.SpectrumUtils
import HasKAL.PlotUtils.PlotUtilsHROOT
import HasKAL.PlotUtils.PlotOption.PlotOptionHROOT
import HasKAL.MonitorUtils.RangeMon.StochMon.StochMon
import HasKAL.DetectorUtils.Detector

{- For fft -}
import Numeric.GSL.Fourier
import Numeric.LinearAlgebra

import HROOT hiding (eval)

main = do

--     ff <- getChannelList "./L-L1_RDS_C03_L2-877264406-128.gwf" 877264406
--     mapM_ putStrLn ff     

     fdata <- readFrame "L1:LSC-STRAIN" "./L-L1_RDS_C03_L2-877264406-128.gwf"
     let dat   = map realToFrac (eval fdata)
         lstxt = take (length dat) [1,2..]

     let lsty  = map snd $ gwpsd dat (163) 16384
         lstxf = map fst $ gwpsd dat (163) 16384
     mapM_ putStrLn (map show (gwpsd dat 163 16384))
     

     logLogPlot lstxf lsty "" "" Line "X11"
--     linearLinearPlot lstxt dat "" "" Line "X11"
--     plot lstxt dat
--     plot_sf lstxf lsty "" "" LogXY Line "test.png"
--     plot_sf lstxf lsty "" "" LogXY Line "X11"


