import Numeric.LinearAlgebra  --subVector
import Numeric --showFFloat
import Control.Monad -- forM
import System.Environment -- getArgs
import System.Random
import System.Process -- system
import Data.List.Split -- splitOn

import HasKAL.MonitorUtils.CorrelationMon.CalCorrelation
import HasKAL.MonitorUtils.CorrelationMon.CorrelationMethod

import HasKAL.FrameUtils.FrameUtils -- read Frame file
import qualified HasKAL.PlotUtils.PlotOption.PlotOptionHROOT as PMOP
import qualified HasKAL.PlotUtils.HROOT.PlotGraph as PM
import qualified HasKAL.PlotUtils.HROOT.PlotGraph3D as PM3
import HasKAL.SpectrumUtils.GwPsdMethod
import HasKAL.SpectrumUtils.SpectrumUtils
import HasKAL.SignalProcessingUtils.Filter
import HasKAL.SignalProcessingUtils.ButterWorth
import HasKAL.SignalProcessingUtils.FilterType

import UsefulFunction

main = do


 let startGPS = 7008 :: Int
     endGPS   = 8000 :: Int
     listGPS' = [startGPS,(startGPS+16)..endGPS]::[Int]
     listGPS  = map show listGPS'
 --print listGPS
 let frameFileName' = map ("/home/yuzurihara/frame/clio/X-R-103465"++) listGPS
     frameFileName  = map (++ "-16.gwf") frameFileName'
 --print frameFileName

 --let fileName = "/home/yuzurihara/frame/clio/X-R-1034657664-16.gwf"


 forM frameFileName $ \fileName -> do
 
  let getGpsTime = read $ (!!2) $ splitOn "-" $ last $ splitOn "/" fileName :: Int
  let channelName = "X1:CTR-PSL_SEIS_IN1_DQ"
  let dfs = 2048 ::Double
  readData1 <- readFrame channelName (fileName)
  let data1  = map realToFrac (eval readData1)
      tt1 = map (/dfs) $ take (length data1) [1,2..]

  let channelName = "X1:CTR-LSC_ETMX_OUT_DQ"
  let dfs = 16384 ::Double
  readData2 <- readFrame channelName (fileName)
  let data2  = map realToFrac (eval readData2)
      tt = map (/dfs) $ take (length data2) [1,2..]

-- resampling from 16384Hz to 2048Hz
-- low-pass filter of cutoff frequency = 1024Hz
-- let dataLPF = butter 4 dfs (2048::Double) Low
  let n=4::Int
  let (numCoeffLow,denomCoeffLow) = butter n dfs 1024.0 Low :: ([Double], [Double])
      dataFiltered = iirFilter data2 numCoeffLow denomCoeffLow
 
-- let fname =  (show n) ++ "_lpf_GWchannel.png"
-- let fname =  "op_" ++ (show n) ++ "_lpf_GWchannel.png"

  let dfs  = 2048.0 ::Double
      maxN = floor $ 0.01 * dfs ::Int
  --print maxN

  result <- forM [0,1..7] $ \rest ->do
   let dataFilteredDownSample = skipListByK 8 rest dataFiltered
   -- calculate correlation value
   --takeCorrelation method x y maxN = case method of
   --let rValueLst = takeCorrelation Peason data1 dataFilteredDownSample maxN
   let rValueLst = takeCorrelation Peason data1 dataFilteredDownSample maxN
   let rValue = maximum rValueLst
   --let rValue_10 = read (showFFloat (Just 10) rValue "")::Double
   let maxValueIndex = correlationResult2pickupMaxValueIndex rValueLst dfs
   let tapleCorrelationValue = (getGpsTime, rValue, rest + (fst maxValueIndex),(fromIntegral rest)/dfs + (snd maxValueIndex))
   --print $ tapleCorrelationValue
   return tapleCorrelationValue

  --print result 
  print $ maximum result
 

