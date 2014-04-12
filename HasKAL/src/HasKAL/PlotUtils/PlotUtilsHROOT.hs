--module PlotUtilsHROOT(
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


hroot_core::[Double] -> [Double] -> String -> String -> LogOption -> PlotTypeOption ->IO()
hroot_core xdata ydata xLabel yLabel flagLog flagPlotLine= do

	   tapp <- newTApplication "test" [0] ["test"]
           tcanvas <- newTCanvas  "Test" "Plot" 640 480
	   g1 <- newTGraph (length xdata) xdata ydata



                
	   draw g1 (checkPlotLine flagPlotLine)
--	   draw g1 "AL*"

           -- let y2 = [4, 3, 2, 1, 6]
	   -- g2 <- newTGraph (length xdata) xdata y2
	   -- draw g2 "L"

           -- 重ね書きはmapを使って実装予定

	   run tapp 1

	   delete g1
	   delete tapp



	   -- Label logスケールのためのフラグは今は捨てているが
	   -- HROOT-0.8を使って実装予定
	   -- label付け、logスケールへの変更の関数をきちんと把握できていないため
	   -- 下に書いた関数はまだ正しくない
	   {--
	   setXLabel 
		| xLabel == "" = " "
		| otherwise = xlabel

           setXLabel
		| xLabel == "" = " "
		  | otherwise = xlabel

           setLogX
                | flagLogX == 1 = 1
                | otherwise = 0

           setLogX
                | flagLogX == 1 = 1
                | otherwise = 0
	   --}



-- 細かいことはいいからplotしたい人向け
plot::[Double] -> [Double] ->IO()
plot xdata ydata = do

	   tapp <- newTApplication "test" [0] ["test"]
           tcanvas <- newTCanvas  "Test" "Plot" 640 480
	   g1 <- newTGraph (length xdata) xdata ydata
	   draw g1 "AL*"
           run tapp 1
	   delete g1
	   delete tapp

-- 時系列データをplotする
-- 時系列なので、logスケールの引数はなし
plot_st::[Double] -> [Double] -> String -> String -> PlotTypeOption ->IO()
plot_st xdata ydata xLabel yLabel flagPlotLine = do

	   tapp <- newTApplication "test" [0] ["test"]
           tcanvas <- newTCanvas  "Test" "Plot" 640 480
	   g1 <- newTGraph (length xdata) xdata ydata
	   draw g1 (checkPlotLine flagPlotLine)
           run tapp 1
	   delete g1
	   delete tapp
-- log-logスケールでスペクトルをplotする
plot_sf::[Double] -> [Double] -> String -> String -> LogOption -> PlotTypeOption ->IO()
plot_sf xdata ydata xLabel yLabel flagLog flagPlotLine= do

	   tapp <- newTApplication "test" [0] ["test"]
           tcanvas <- newTCanvas  "Test" "Plot" 640 480
	   g1 <- newTGraph (length xdata) xdata ydata

           -- ここでlogにする HROOT-0.8を使って実装予定

	   draw g1 (checkPlotLine flagPlotLine)
           run tapp 1
	   delete g1
	   delete tapp





checkPlotLine :: PlotTypeOption -> String
checkPlotLine flagPlotLine
        | flagPlotLine == Line      = "AL"
        | flagPlotLine == Point     = "AP*"
        | flagPlotLine == LinePoint = "AL*"
        | flagPlotLine == PointLine = "AL*"
        | flagPlotLine == Dot       = "AP"
        | otherwise                 = "AL"


