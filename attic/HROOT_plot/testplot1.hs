
import HasKAL.PlotUtils.PlotUtilsHROOT
import HasKAL.PlotUtils.PlotOption.PlotOptionHROOT

import HROOT

main = do

     let x = [0.0, 0.1 .. 2*pi]
     let y = map (sin) x

     tapp <- newTApplication "test" [0] ["test"] 

     plot_st x y "" "" Line "hoge.png" tapp

     let z = map (cos) x
     plot_st x z "" "" Line "foo.png" tapp

     plot_st x y "" "" Line "X11" tapp

     delete tapp
