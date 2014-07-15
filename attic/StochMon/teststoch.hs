import HasKAL.MonitorUtils.RangeMon.StochMon.StochMon
import HasKAL.PlotUtils.PlotUtilsHROOT
import HasKAL.PlotUtils.PlotOption.PlotOptionHROOT
import HasKAL.DetectorUtils.Detector

main = do
     let x = [10..200]
         y = h2omega_sens_allf (60*60*24*365*3) LIGO_Hanford LIGO_Livingston 0.05 0.95 x	
     let outy = zipWith (\x y -> show x ++ "   " ++ show y) x y
     mapM_ putStrLn outy
     logLogPlot x y "" "" Line "X11"




