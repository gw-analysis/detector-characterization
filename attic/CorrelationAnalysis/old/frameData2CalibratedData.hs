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

import qualified HasKAL.PlotUtils.PlotOption.PlotOptionHROOT as PMOP
import qualified HasKAL.PlotUtils.HROOT.PlotGraph as PM
import qualified HasKAL.PlotUtils.HROOT.PlotGraph3D as PM3
--import qualified HasKAL.PlotUtils.HROOT.Histogram as H
import qualified HasKAL.PlotUtils.HROOT.SignalHandlerHROOT as RSH

import qualified HasKAL.SpectrumUtils.SpectrumUtils as SU
--import qualified HasKAL.SignalProcessingUtils.Interpolation as SPUI
--import qualified HasKAL.SignalProcessingUtils.InterpolationType as SPUIT

main = do

 -- read data of 
 trfData' <- readFile "./modG_factor.txt"
 let trfDataf = map string2Double $ map (!!0) $ map words $ lines trfData'
 let trfDataG = map string2Double $ map (!!1) $ map words $ lines trfData'
 --print $ zip trfDataf trfDataG

 let startGPS = 456 :: Int
     endGPS   = 696 :: Int
     listGPS' = [startGPS,(startGPS+16)..endGPS]::[Int]
     listGPS  = map show listGPS'
 --print listGPS
 let frameFileName' = map ("/home/yuzurihara/frame/clio/X-R-1034657"++) listGPS
     frameFileName  = map (++ "-16.gwf") frameFileName'
 print frameFileName


 forM frameFileName $ \fileName -> do

  let getGpsTime = read $ (!!2) $ splitOn "-" $ last $ splitOn "/" fileName :: Double

  -- plot non-calibrated GW channel in time domain
  let channelName = "X1:CTR-LSC_ETMX_OUT_DQ"
  let dfs = 16384 ::Double
  readData3 <- readFrame channelName (fileName)
  let data3  = map realToFrac (eval readData3)
      xdata3 = map (/dfs) $ take (length data3) [1,2..]
      plotdata3 = zip xdata3 data3
  

  -- using FFT, spectrum of non-calibrated GW channel
  -- gwpsd :: [Double]-> Int -> Double -> [(Double, Double)]
  -- gwpsd dat nfft fs = gwpsdCore Welch dat nfft fs Hann
  let dataSpecGW = SU.gwpsd data3 (length data3) dfs
      fname =  (showFFloat (Just 0) getGpsTime "" ) ++ "_" ++ (channelName) ++ "_GWchannel_sf.png"
      channelName' = (channelName) ++ "__" ++ (showFFloat (Just 0) getGpsTime "" ) :: String
  --PM.plot PMOP.LogXY PMOP.Line ("frequency [Hz]","interporated transfer function") channelName' fname dataSpecGW


  -- using transfer function, calculate calibrated spectrum
  let dataSpecGW' = take (length trfDataf) dataSpecGW
      dataCalibSpect = zipWith (*) (map snd dataSpecGW') trfDataG
      dataCalibSpectSet = zip trfDataf dataCalibSpect ::[(Double, Double)]
      fname = (showFFloat (Just 0) getGpsTime "" ) ++ "_calibrated_spect.txt"

  writeFile fname $ taple2string trfDataf dataCalibSpect
  print "hoge"  


taple2string ::[Double] -> [Double] -> String
taple2string a b = unlines  $ zipWith (++) (map show a)  $ zipWith (++) (repeat " ")  (map show b)

string2Int :: String -> Int
string2Int str = read str::Int

string2Double :: String -> Double
string2Double str = read str::Double

