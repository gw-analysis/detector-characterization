
{--

----- To compile -----
You need to create symbolic link to HasKAL.
    ln -s ~/detector-characterization/HasKAL/src/HasKAL ./

You need to create symbolic link to frame data.
    ln -s ~/detector-characterization/test/sample-data/test-1066392016-300.gwf ./

After these command, you can compile.
If you have any trouble or error, let author(Hirotaka Yuzurihara) know please.

--}

{--
----- sample code -----

import HasKAL.PlotUtils.PlotUtilsHROOT

main = do

     let x = [0.0, 0.1 .. 2*pi]
     let y = map (sin) x

     hroot_core x y "" "" Linear LinePoint
     --plot x y
     plot_st x y "" "" LinePoint

--}


module HasKAL.PlotUtils.PlotUtilsHROOT(
       PlotTypeOption(Line, Point, LinePoint, PointLine, Dot),
       LogOption(Linear, LogX, LogY, LogXY),
       hroot_core,
       plot,
       plot_st,
       plot_sf
       ) where

import HROOT

data PlotTypeOption = Line | Point | LinePoint | PointLine | Dot deriving Eq
data LogOption = Linear | LogX | LogY | LogXY deriving Eq


hroot_core::[Double] -> [Double] -> String -> String -> LogOption -> PlotTypeOption -> IO()
hroot_core xdata ydata xLabel yLabel flagLog flagPlotLine = do

	   tapp <- newTApplication "test" [0] ["test"]
           tcanvas <- newTCanvas  "Test" "Plot" 640 480

	   g1 <- newTGraph (length xdata) xdata ydata

           let checkLogX :: LogOption -> Int
               checkLogX flagLog
                       | flagLog == Linear = 0
                       | flagLog == LogX   = 1
                       | flagLog == LogY   = 0
                       | flagLog == LogXY  = 1
                       | otherwise         = 0

           let checkLogY :: LogOption -> Int
               checkLogY flagLog
                       | flagLog == Linear = 0
                       | flagLog == LogX   = 0
                       | flagLog == LogY   = 1
                       | flagLog == LogXY  = 1
                       | otherwise         = 0

           setLogx tcanvas (checkLogX flagLog)
           setLogy tcanvas (checkLogY flagLog)


           let checkPlotLine :: PlotTypeOption -> String
               checkPlotLine flagPlotLine
                       | flagPlotLine == Line      = "AL"
                       | flagPlotLine == Point     = "AP*"
                       | flagPlotLine == LinePoint = "AL*"
                       | flagPlotLine == PointLine = "AL*"
                       | flagPlotLine == Dot       = "AP"
                       | otherwise                 = "AL"


	   draw g1 (checkPlotLine flagPlotLine)

	   run tapp 1

	   delete g1
           delete tcanvas
	   delete tapp

--hroot_core xdata ydata xLabel yLabel flagLog flagPlotLine

-- 細かいことはいいからplotしたい人向け
plot::[Double] -> [Double] ->IO()
plot xdata ydata = do
           hroot_core xdata ydata "" "" Linear LinePoint

-- 時系列データをplotする
-- 時系列なので、logスケールの引数はなし
plot_st::[Double] -> [Double] -> String -> String -> PlotTypeOption ->IO()
plot_st xdata ydata xLabel yLabel flagPlotLine = do
           hroot_core xdata ydata "" "" Linear flagPlotLine

-- log-logスケールでスペクトルをplotする
plot_sf::[Double] -> [Double] -> String -> String -> LogOption -> PlotTypeOption ->IO()
plot_sf xdata ydata xLabel yLabel flagLog flagPlotLine= do
           hroot_core xdata ydata "" "" flagLog flagPlotLine








