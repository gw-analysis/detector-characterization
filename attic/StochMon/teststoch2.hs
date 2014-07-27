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

     ff <- getChannelList "/data/L-L1_RDS_C03_L2-8772/L-L1_RDS_C03_L2-877232395-36.gwf" 
     mapM_ print ff     

     fdata <- readFrame "L1:LSC-STRAIN" "/data/L-L1_RDS_C03_L2-8772/L-L1_RDS_C03_L2-877232395-36.gwf"
     let dat   = map realToFrac (eval fdata)
         lstxt = map (/16384) $ take (length dat) [1,2..]
     mapM_ print dat

     let lsty  = map snd $ gwpsd dat 128 16384
         lstxf = map fst $ gwpsd dat 128 16384
     mapM_ putStrLn (map show (gwpsd dat 128 16384))
     

--     logLogPlot lstxf lsty "" "" Line "X11"
--     linearLinearPlot lstxt dat "" "" Line "X11"
--     plot lstxt dat
--     plot_sf lstxf lsty "" "" LogXY Line "test.png"
--     plot_sf lstxf lsty "" "" LogXY Line "X11"


