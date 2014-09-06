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

 let fs = "128"::String


 [frameFileName] <- getArgs
 print $ frameFileName
 let getGpsTime = read $ (!!2) $ splitOn "-" $ last $ splitOn "/" frameFileName :: Double
 print getGpsTime
 
 -- read Channel List
 --channelList <- getChannelList frameFileName
 --print channelList

 let channelName = "X1:CTR-PSL_SEIS_IN1_DQ" ::String
 let ifs = 2048 :: Int
     ifs_harf = floor $ (fromIntegral ifs)/4.0
     dfs = 2048.0 :: Double


 --readFrame :: String -> String -> IO(FrDataType [CDouble])
 readData1 <- readFrame channelName (frameFileName)
 let dataChannel  = map realToFrac (eval readData1)
     xdata1  = map (/dfs) $ take (length dataChannel) [1,2..]

     --xdata1' = map (+ getGpsTime) xdata1
 let plotdata = zip xdata1 (dataChannel)


 RSH.addSignalHandle
 
 --PM.plotX PM.Linear PM.Line ("time[sec]","s(t)") "channelName" plotdata

 --spectrogram plot
 let dataSpect = SU.gwspectrogram ifs_harf ifs dfs dataChannel
 
 --print dataSpect
 let fname =  (showFFloat (Just 0) getGpsTime "" ) ++ "_" ++ (channelName) ++ ".png"
 PM3.spectrogram LogYZ COLZ " " channelName fname dataSpect

 print "hoge"
