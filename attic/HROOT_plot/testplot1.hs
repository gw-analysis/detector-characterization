
import HasKAL.PlotUtils.PlotUtilsHROOT
import HasKAL.PlotUtils.PlotOption.PlotOptionHROOT

import Control.Concurrent

main = do
     let x = [0.0, 0.1 .. 800.0]
     let y = map (sin) x


--     plot_st x y "" "" Line "hoge.png"

--     let z = map (cos) x
--     plot_st x z "" "" Line "foo.png"
     linearLinearPlot x y "" "" Dot "X11"
--     plot_st x y "" "" Line "X11"

