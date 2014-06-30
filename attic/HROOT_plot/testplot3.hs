
import HasKAL.PlotUtils.PlotUtilsHROOT
import HasKAL.PlotUtils.PlotOption.PlotOptionHROOT
import HasKAL.DetectorUtils.Detector
import HasKAL.SpectrumUtils.DetectorSensitivity
import Numeric.LinearAlgebra

import HROOT

main = do

       let f = [1,2..1000]
       let sn = toList $ sqrt $ ifonoisepsd KAGRA (fromList [1,2..1000])

       logLogPlot f sn "" "" Line "X11"

