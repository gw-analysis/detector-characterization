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
import HasKAL.MonitorUtils.CorrelationMon.CorrelationMethod
import HasKAL.FrameUtils.FrameUtils -- read Frame file
--import HasKAL.PlotUtils.PlotUtilsHROOT
--import HasKAL.PlotUtils.PlotOption.PlotOptionHROOT


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
  

   let rValue = maximum $ takeCorrelation Peason data1 data2 1
   let rValue_10 = read (showFFloat (Just 10) rValue "")::Double

   {-
   let outputFile = "pic__" ++ (show getGpsTime) ++ "__" ++(show rValue_10) ++  "__" ++ channel1 ++ "___" ++ channel2 ++ ".png"
   plotSaveAsPicture data1 data2 "" "" Linear Dot outputFile
   -}
   
   --return $ maximum $ twoChannelData2Correlation data1 data2 1 :: IO Double
   --return $ ( read (showFFloat (Just 10) rValue "") ::Double)

   return $ (showFFloat (Just 10) rValue "" ) ++ " " ++ channel1 ++ " " ++ channel2
  return result'


 print $ concat result
 
 let resultFile = "result__" ++ (show getGpsTime) ++ ".txt"
 writeFile resultFile $ unlines $ map show $ concat result

