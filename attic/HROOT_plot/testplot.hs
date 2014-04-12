import HasKAL.FrameUtils.FrameUtils
import HasKAL.PlotUtils.PlotUtilsHROOT
--PlotUtilsHROOT.hs 
--import FrameUtils
import HROOT hiding (eval)

main = do

     let a = [1,2,3,4,5]
     let b = [5, 2, 3, 4, 5]
--     hroot_core a b "" "" Linear LinePoint
--     plot a b
     plot_st a b "" "" LinePoint



     -- let c = [10, 11, 12, 13, 14, 15, 16]
     -- let d = [5, 2, 3, 4, 5, 5, 2]
     -- hroot_core c d "" "" Linear LinePoint
