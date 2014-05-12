
import HasKAL.PlotUtils.PlotUtilsHROOT
import HasKAL.PlotUtils.PlotOption.PlotOptionHROOT
import HasKAL.DetectorUtils.Detector
import HasKAL.SpectrumUtils.DetectorSensitivity
import Numeric.LinearAlgebra

import HROOT

main = do

       tapp <- newTApplication "test" [0] ["test"] 

       let f = [1,2..1000]
       let sn = toList $ sqrt $ ifonoisepsd KAGRA (fromList [1,2..1000])

       plot_sf f sn "" "" LogXY Line "X11" tapp

       delete tapp
