-- test code of HasKAL.MonitorUtils.CorrelationMon.CalCorrelation
-- read 1 frame data and for each channel data, calculate correlation value.


-- usage: Neew one argument (Frame File)
-- check_correlation /data/ligo/archive/frames/S6/L1/LLO/L-L1_RDS_R_L1-9592/L-L1_RDS_R_L1-959200000-64.gwf

-- First you must execute below command
-- setupChannelList.sh


import Numeric.LinearAlgebra  --subVector
import Numeric --showFFloat
import Control.Monad -- forM
import System.Environment -- getArgs
import System.Cmd -- system
import Data.List.Split -- splitOn

import HasKAL.MonitorUtils.CorrelationMon.CalCorrelation
import HasKAL.FrameUtils.FrameUtils -- read Frame file
--import HasKAL.PlotUtils.PlotUtilsHROOT
--import HasKAL.PlotUtils.PlotOption.PlotOptionHROOT

import Data.Text (Text)
import qualified Data.Text as T 
import qualified Data.Text.IO as T
import System.Random

import Prelude hiding (id,(.))
import Control.Category


writeToHtml :: FilePath -> String -> IO ()
writeToHtml path result = writeFile path $ concat ["<html> <head><body> <div id='body'></div> <script src='http://d3js.org/d3.v3.min.js' charset='utf-8'></script> <script type=\"text/javascript\">\n","var width  = 600;\nvar height = 600;\nvar barPadding = 1;\nvar nChannel = 6;\nvar simulationMax = nChannel * nChannel;\n","var dataset = ", result,";\n","var svg = d3.select(\"body\").append(\"svg\").attr(\"width\", width).attr(\"height\", height);\nsvg.selectAll(\"rect\").data(dataset).enter().append(\"rect\").attr(\"x\",function(d, i) {return (height / nChannel ) * (i % nChannel);}) .attr(\"y\", function(d, i){return ((height / nChannel ) * Math.floor( i / nChannel));}).attr(\"width\", function(d, i){return (height / nChannel - 1 );}).attr(\"height\", function(d, i){return (height / nChannel - 1 );}).attr(\"fill\", function(d) {return \"rgb(0, 0, \" + (Math.floor((d+1.0)*255))  + \")\";});\nsvg.selectAll(\"text\").data(dataset).enter().append(\"text\").text(function(d) {return d.toFixed(2);}).attr(\"text-anchor\", \"middle\").attr(\"x\", function(d, i) {return (height / nChannel ) * (i % nChannel) + height / nChannel /2.0;}).attr(\"y\", function(d, i){return ((height / nChannel ) * Math.floor( i / nChannel)) + height / nChannel / 2.0;}).attr(\"width\", function(d, i){return (height / nChannel - 1 );}).attr(\"height\", function(d, i){return (height / nChannel - 1 );}).attr(\"font-family\", \"Arial\").attr(\"font-size\", \"18px\").attr(\"fill\", \"white\");\n","for(var i=0; i < dataset.length ; i++){d3.select(\"body\").selectAll(\"p\").data(dataset).enter().append(\"p\").text(function(d) {return d});}" ,"\n</script>\n </body>\n </html>"]



--main IO()
main = do


 -- now these channel list is written explicitly.
 -- but future work remove these channel list and in this source, channel list is calculated.
 -- /usr/bin/FrChannels
-- let channelList = ["L1:OMC-TT1_SUSYAW_IN1_DAQ", "L1:OMC-TT2_SUSYAW_IN1_DAQ", "L1:OMC-TT1_SUSPIT_IN1_DAQ","L1:OMC-TT2_SUSPIT_IN1_DAQ","L1:OMC-TT1_SUSPOS_IN1_DAQ","L1:OMC-TT2_SUSPOS_IN1_DAQ"]


 -- how size slide
 let fs = "128"::String


 [frameFileName] <- getArgs
 print $ frameFileName
 let getGpsTime = read $ (!!2) $ splitOn "-" $ last $ splitOn "/" frameFileName :: Integer
 --print gpsTime
 

 -- read Channel List
 channelList'' <- readFile "channelList.txt"
 let channelList' = map words $ lines channelList''
 let channelList  = map (!!0) channelList'
 --channelList <- getChannelList frameFileName 959200000


 -- calculate correlation value
 result <- forM channelList $ \channel1 -> do

  readData1 <- readFrame channel1 (frameFileName)
  let data1  = map realToFrac (eval readData1)
      xdata1  = take (length data1) [1,2..]

  {-
  writeFile "tmp.txt" $ show (eval readData1)
  let doCommand = "/bin/mv tmp.txt" ++ " " ++ channel1 ++ ".txt"
  system doCommand
  -}

  result' <- forM channelList $ \channel2 -> do

   readData2 <- readFrame channel2 (frameFileName)
   let data2 = map realToFrac (eval readData2)
       xdata2 = take (length data2) [1,2..]
  

   let rValue = maximum $ twoChannelData2Correlation data1 data2 1
   let rValue_10 = read (showFFloat (Just 10) rValue "")::Double

   {-
   let outputFile = "pic__" ++ (show getGpsTime) ++ "__" ++(show rValue_10) ++  "__" ++ channel1 ++ "___" ++ channel2 ++ ".png"
   plotSaveAsPicture data1 data2 "" "" Linear Dot outputFile
   -}
   
   --return $ maximum $ twoChannelData2Correlation data1 data2 1 :: IO Double
   --return $ ( read (showFFloat (Just 10) rValue "") ::Double)

   --return $ (showFFloat (Just 10) rValue "" ) ++ " " ++ channel1 ++ " " ++ channel2
   return $ (read (showFFloat (Just 10) rValue "" )::Double)
  return result'


 print $ concat result
 
 let resultFile = "result__" ++ (show getGpsTime) ++ ".txt"
 writeFile resultFile $ unlines $ map show $ concat result


 writeToHtml "exampleResult.html" ( show $ concat result)

