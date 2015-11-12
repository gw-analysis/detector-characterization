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
import System.Random
import System.Process -- system
import Data.List.Split -- splitOn

import HasKAL.FrameUtils.FrameUtils -- read Frame file

import qualified HasKAL.PlotUtils.PlotOption.PlotOptionHROOT as PMOP
import qualified HasKAL.PlotUtils.HROOT.PlotGraph as PM
import qualified HasKAL.PlotUtils.HROOT.PlotGraph3D as PM3
import HasKAL.SpectrumUtils.GwPsdMethod
import HasKAL.SpectrumUtils.SpectrumUtils
import HasKAL.SignalProcessingUtils.Filter
import HasKAL.SignalProcessingUtils.ButterWorth
import HasKAL.SignalProcessingUtils.FilterType

main = do

 -- check of some filter
 forM [2..10] $ \n -> do
  let x = take 1000 $ randomRs (-1, 1) $ mkStdGen 1 :: [Double]
      (numCoeffLow,denomCoeffLow)   = butter n 100 20 Low ::([Double], [Double])
      (numCoeffHigh,denomCoeffHigh) = butter n 100 20 High ::([Double], [Double])
      y = iirFilter x numCoeffLow denomCoeffLow
      z = iirFilter x numCoeffHigh denomCoeffHigh

  let out1 = gwpsd x 100 100.0
      out2 = gwpsd y 100 100.0
      out3 = gwpsd z 100 100.0

  -- plot filtered data
  let fname = "raw_n_" ++ (show n) ++ ".png"
  PM.plot PM.LogXY PM.Line ("frequency [Hz]", "") "no filter" fname        out1
  let fname = "lpf_n_" ++ (show n) ++ ".png"
  PM.plot PM.LogXY PM.Line ("frequency [Hz]", "") "low pass filter" fname  out2
  let fname = "hpf_n_" ++ (show n) ++ ".png"
  PM.plot PM.LogXY PM.Line ("frequency [Hz]", "") "hish pass filter" fname out3

 let startGPS = 456 :: Int
     endGPS   = 696 :: Int
     listGPS' = [startGPS,(startGPS+16)..endGPS]::[Int]
     listGPS  = map show listGPS'
 --print listGPS
 let frameFileName' = map ("/home/yuzurihara/frame/clio/X-R-1034657"++) listGPS
     frameFileName  = map (++ "-16.gwf") frameFileName'
 let fileName  = frameFileName!!0

 --get GPS time 
 let getGpsTime = read $ (!!2) $ splitOn "-" $ last $ splitOn "/" fileName :: Double
 print $ (showFFloat (Just 0) getGpsTime "" )


 -- plot non-calibrated GW channel in time domain
 let channelName = "X1:CTR-LSC_ETMX_OUT_DQ"
 let dfs = 16384 ::Double
 readData3 <- readFrame channelName (fileName)
 let data3  = map realToFrac (eval readData3)
     out4 = gwpsd data3 (length data3) dfs
 let fname = "no_filter_spect.png"
 PM.plot PM.LogXY PM.Line ("frequency [Hz]", "") " " fname out4

-- resampling from 16384Hz to 2048Hz
-- low-pass filter of cutoff frequency = 1024Hz
-- let dataLPF = butter 4 dfs (2048::Double) Low
 forM [2..10] $ \n -> do
  let (numCoeffLow,denomCoeffLow) = butter n dfs 1024.0 Low :: ([Double], [Double])
      dataFiltered = iirFilter data3 numCoeffLow denomCoeffLow
      out4 = gwpsd dataFiltered (length dataFiltered) dfs

  let fname = "filtered_spectrum_n_" ++ (show n) ++ ".png"
  PM.plot PM.LogXY PM.Line ("frequency [Hz]", "") "LPF" fname out4


  print "hoge"
  

string2Int :: String -> Int
string2Int str = read str::Int

string2Double :: String -> Double
string2Double str = read str::Double
