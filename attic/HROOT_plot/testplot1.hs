
import HasKAL.PlotUtils.PlotUtilsHROOT
import HasKAL.PlotUtils.PlotOption.PlotOptionHROOT

main = do
     let x = [0.0, 0.1 .. 2*pi]
     let y = map (sin) x

     plot_st x y "" "" Line "hoge.png"

--     let z = map (cos) x
--     plot_st x z "" "" Line "foo.png"
--     plot_st x y "" "" Line "X11"
