import HasKAL.PlotUtils.PlotUtilsHROOT

--import Control.Monad
--import HROOT

main = do


     let x = [0.0, 0.1 .. 2*pi]
     let y = map (sin) x

     plot_st x y "" "" Line
--     plot_st x y "" "" LinePoint

     --hroot_core x y "" "" Linear LinePoint
     --plot x y
