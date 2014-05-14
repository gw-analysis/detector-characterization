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
       hroot_core,
       plot,
       plot_st,
       plot_sf
       ) where

import HROOT
import HasKAL.PlotUtils.PlotOption.PlotOptionHROOT

hroot_core::[Double] -> [Double] -> String -> String -> LogOption -> PlotTypeOption -> String ->IO()
hroot_core xdata ydata xLabel yLabel flagLog flagPlotLine fileName = do

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

           let checkSaveAsOrPlotX11 :: String -> IO()
               checkSaveAsOrPlotX11 fileName
                                    | (reverse $ take 4 $ reverse fileName) == ".png" = saveAs tcanvas fileName ""
                                    | (reverse $ take 4 $ reverse fileName) == ".pdf" = saveAs tcanvas fileName ""
                                    | (reverse $ take 4 $ reverse fileName) == ".eps" = saveAs tcanvas fileName ""
                                    | otherwise = run tapp 1

           checkSaveAsOrPlotX11 fileName


	   delete g1
           delete tcanvas
	   delete tapp




-- 細かいことはいいからplotしたい人向け
plot::[Double] -> [Double] -> String ->IO()
plot xdata ydata fileName = do
           hroot_core xdata ydata "" "" Linear LinePoint fileName


-- 時系列データをplotする
-- 時系列なので、logスケールの引数はなし
plot_st::[Double] -> [Double] -> String -> String -> PlotTypeOption -> String -> IO()
plot_st xdata ydata xLabel yLabel flagPlotLine fileName = do
           hroot_core xdata ydata "" "" Linear flagPlotLine fileName


-- log-logスケールでスペクトルをplotする
plot_sf::[Double] -> [Double] -> String -> String -> LogOption -> PlotTypeOption -> String -> IO()
plot_sf xdata ydata xLabel yLabel flagLog flagPlotLine fileName = do
           hroot_core xdata ydata "" "" flagLog flagPlotLine fileName








