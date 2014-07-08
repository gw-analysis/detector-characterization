-- 2つのデータをそれぞれ読み込む
-- charのリストをDoubleのリストに変換
-- correlation の値を計算する

import Numeric.LinearAlgebra  --subVector
import Numeric --showFFloat
import Control.Monad -- forM
import System.Random
import System.Cmd

import HROOT hiding (eval)

import HasKAL.MonitorUtils.CorrelationMon.CalCorrelation
--import HasKAL.MonitorUtils.CorrelationMon.CalCorrelation
import HasKAL.FrameUtils.FrameUtils -- read Frame file
import HasKAL.PlotUtils.PlotUtilsHROOT
import HasKAL.PlotUtils.PlotOption.PlotOptionHROOT



main :: IO()
main = do


 let channelList = ["L1:OMC-TT1_SUSYAW_IN1_DAQ", "L1:OMC-TT2_SUSYAW_IN1_DAQ", "L1:OMC-TT1_SUSPIT_IN1_DAQ","L1:OMC-TT2_SUSPIT_IN1_DAQ","L1:OMC-TT1_SUSPOS_IN1_DAQ","L1:OMC-TT2_SUSPOS_IN1_DAQ"]


 let fs = 128
 let duration = 64


 forM_ channelList $ \channel1 -> do

  readData1 <- readFrame channel1 "./L-L1_RDS_R_L1-959200000-64.gwf"
  let data1  = map realToFrac (eval readData1)
      xdata1  = take (length data1) [1,2..]

  forM_ channelList $ \channel2 -> do

   readData2 <- readFrame channel2 "./L-L1_RDS_R_L1-959200000-64.gwf"
   let data2 = map realToFrac (eval readData2)
       xdata2 = take (length data2) [1,2..]


   let rValue = maximum $ twoChannelData2Correlation data1 data2 1
   --let rValue_10 = showFFloat (Just 10) rValue ""
   print rValue

   
  
   -- ##### plot in format : png
   

   --linearLinearPlot xdata1 data1 "" "" Line "png"
   {--
   tcanvas <- newTCanvas  "Test" "Plot" 640 480
   g1 <- newTGraph (length (min data1 data2)) data1 data2
   draw g1 "AP"
   saveAs tcanvas "hoge.png" ""
   delete g1
   delete tcanvas
   let doCommand = "/bin/mv hoge.png" ++ " " ++ channel1 ++ "___" ++ channel2 ++ ".png"
   system doCommand
   --}
