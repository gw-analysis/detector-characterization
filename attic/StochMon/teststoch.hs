import HasKAL.MonitorUtils.RangeMon.StochMon.StochMon
import HasKAL.PlotUtils.PlotUtilsHROOT
import HasKAL.PlotUtils.PlotOption.PlotOptionHROOT
import HasKAL.DetectorUtils.Detector

main = do
     let x = [0..1000]
     let y = h2omega_allf 10000000 KAGRA VIRGO 0.05 0.95
     logLogPlot x y "" "" Line "X11"




