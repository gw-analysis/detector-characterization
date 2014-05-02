
import HasKAL.PlotUtils.PlotUtilsHROOT
import HasKAL.DetectorUtils.Detector
import HasKAL.SpectrumUtils.DetectorSensitivity
import Numeric.LinearAlgebra

main = do


       let f = [1,2..1000]
       let sn = toList $ sqrt $ ifonoisepsd KAGRA (fromList [1,2..1000])
       plot_sf f sn "" "" LogXY Line

