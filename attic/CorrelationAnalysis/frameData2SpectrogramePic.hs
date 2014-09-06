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
--import System.Cmd -- system
import System.Process -- system
import Data.List.Split -- splitOn

import HasKAL.MonitorUtils.CorrelationMon.CalCorrelation
import HasKAL.MonitorUtils.CorrelationMon.CorrelationMethod
import HasKAL.FrameUtils.FrameUtils -- read Frame file

import HasKAL.PlotUtils.PlotOption.PlotOptionHROOT
import qualified HasKAL.PlotUtils.HROOT.PlotGraph as PM
import qualified HasKAL.PlotUtils.HROOT.PlotGraph3D as PM3
--import qualified HasKAL.PlotUtils.HROOT.Histogram as H
import qualified HasKAL.PlotUtils.HROOT.SignalHandlerHROOT as RSH

import qualified HasKAL.SpectrumUtils.SpectrumUtils as SU

main = do

 let frameFileName = ["/home/yuzurihara/frame/clio/X-R-1034657456-16.gwf","/home/yuzurihara/frame/clio/X-R-1034657584-16.gwf","/home/yuzurihara/frame/clio/X-R-1034657472-16.gwf","/home/yuzurihara/frame/clio/X-R-1034657600-16.gwf","/home/yuzurihara/frame/clio/X-R-1034657488-16.gwf","/home/yuzurihara/frame/clio/X-R-1034657616-16.gwf","/home/yuzurihara/frame/clio/X-R-1034657504-16.gwf","/home/yuzurihara/frame/clio/X-R-1034657632-16.gwf","/home/yuzurihara/frame/clio/X-R-1034657520-16.gwf","/home/yuzurihara/frame/clio/X-R-1034657648-16.gwf","/home/yuzurihara/frame/clio/X-R-1034657536-16.gwf","/home/yuzurihara/frame/clio/X-R-1034657664-16.gwf","/home/yuzurihara/frame/clio/X-R-1034657552-16.gwf","/home/yuzurihara/frame/clio/X-R-1034657680-16.gwf","/home/yuzurihara/frame/clio/X-R-1034657568-16.gwf","/home/yuzurihara/frame/clio/X-R-1034657696-16.gwf"]
 print frameFileName

 let channelName = "X1:CTR-PSL_SEIS_IN1_DQ" ::String
 let ifs = 2048 :: Int
     ifs_harf = floor $ (fromIntegral ifs)/4.0
     dfs = 2048.0 :: Double

 forM frameFileName $ \fileName -> do
  print fileName
  readData1 <- readFrame channelName (fileName)
  let getGpsTime = read $ (!!2) $ splitOn "-" $ last $ splitOn "/" fileName :: Double
  print $ (showFFloat (Just 0) getGpsTime "" )
  let dataChannel  = map realToFrac (eval readData1)

  -- spectrogram
  let dataSpect = SU.gwspectrogram ifs_harf ifs_harf dfs dataChannel
  let fname =  (showFFloat (Just 0) getGpsTime "" ) ++ "_" ++ (channelName) ++ ".png"
  let channelName' = (channelName) ++ "__" ++ (showFFloat (Just 0) getGpsTime "" ) :: String
  PM3.spectrogram LogYZ COLZ " " channelName' fname dataSpect

  -- plot s(t)
  let xdata1  = map (/dfs) $ take (length dataChannel) [1,2..]
      plotdata1 = zip xdata1 (dataChannel)
  let fname =  (showFFloat (Just 0) getGpsTime "" ) ++ "_" ++ (channelName) ++ "_vt.png"
  let channelName' = (channelName) ++ "__" ++ (showFFloat (Just 0) getGpsTime "" ) :: String
  PM.plot PM.Linear PM.Line ("time[sec]","s(t)") channelName' fname plotdata1


  -- plot lock flag
  --let channelName = "X1:CTR-LSC_TRX_OUT16"
  --let dfs = 16 ::Double
  let channelName = "X1:CTR-LSC_TRX_OUT_DQ"
  let dfs = 16384 ::Double
  readData2 <- readFrame channelName (fileName)
  let data2  = map realToFrac (eval readData2)
      xdata2 = map (/dfs) $ take (length data2) [1,2..]
      plotdata2 = zip xdata2 data2
  let fname =  (showFFloat (Just 0) getGpsTime "" ) ++ "_" ++ (channelName) ++ "_lock_flag.png"
  let channelName' = (channelName) ++ "__" ++ (showFFloat (Just 0) getGpsTime "" ) :: String
  PM.plot PM.Linear PM.Line ("time[sec]","lock flag") channelName' fname plotdata2

  -- plot GW channel
  let channelName = "X1:CTR-LSC_ETMX_OUT_DQ"
  let dfs = 16384 ::Double
  readData3 <- readFrame channelName (fileName)
  let data3  = map realToFrac (eval readData3)
      xdata3 = map (/dfs) $ take (length data3) [1,2..]
      plotdata3 = zip xdata3 data3
  let fname =  (showFFloat (Just 0) getGpsTime "" ) ++ "_" ++ (channelName) ++ "_GWchannel.png"
  let channelName' = (channelName) ++ "__" ++ (showFFloat (Just 0) getGpsTime "" ) :: String
  PM.plot PM.Linear PM.Line ("time[sec]","GW channel") channelName' fname plotdata3
