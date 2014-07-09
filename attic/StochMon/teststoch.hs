import HasKAL.MonitorUtils.RangeMon.StochMon.StochMon
import HasKAL.PlotUtils.PlotUtilsHROOT
import HasKAL.PlotUtils.PlotOption.PlotOptionHROOT
import HasKAL.DetectorUtils.Detector

main = do
     let x = [1..100]
     let y = h2omega_sens_allf 10000000 KAGRA VIRGO 0.05 0.95 x
     logLogPlot x y "" "" Line "X11"




