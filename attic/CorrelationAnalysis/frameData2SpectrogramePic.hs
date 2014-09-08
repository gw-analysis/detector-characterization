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
import HasKAL.SpectrumUtils.GwPsdMethod
import HasKAL.SignalProcessingUtils.Filter
import HasKAL.SignalProcessingUtils.ButterWorth
import HasKAL.SignalProcessingUtils.FilterType

main = do

 -- read data of 
 trfData' <- readFile "./modG_factor.txt"
 let trfDataf = map string2Double $ map (!!0) $ map words $ lines trfData'
 let trfDataG = map string2Double $ map (!!1) $ map words $ lines trfData'
 --print $ zip trfDataf trfDataG

 -- interporate of transfer function -> not necessaly?
 -- interp xx yy interp_xx interpType    
 --let dataInterpFreq = map [1..
 --print $ SPUI.interp trfDataf trfDataG 5.0 SPUIT.Linear
 --map SPUI.interp trfDataf trfDataG 5.0 SPUIT.Linear
 --PM.plotO PM.Linear PM.Point ("frequency [Hz]","interporated transfer function") "hoge" plotdata2


 -- plot transfer function, check
 let fname = "CLIO_transfer_function.png" ::String
 let channelName = "CLIO_transfer_function" ::String
 PM.plot PM.LogXY PM.Line ("freqnency [Hz]","transfer function") channelName fname $ zip trfDataf trfDataG

 let startGPS = 7632 :: Int
     endGPS   = 7696 :: Int
     listGPS' = [startGPS,(startGPS+16)..endGPS]::[Int]
     listGPS  = map show listGPS'
 --print listGPS
 let frameFileName' = map ("/home/yuzurihara/frame/clio/X-R-103465"++) listGPS
     frameFileName  = map (++ "-16.gwf") frameFileName'
 print frameFileName

 let channelName = "X1:CTR-PSL_SEIS_IN1_DQ" ::String
 let ifs = 2048 :: Int
     ifs_harf = floor $ (fromIntegral ifs)/2.0
     dfs = 2048.0 :: Double

 --let frameFileName = ["/home/yuzurihara/frame/clio/X-R-1034657488-16.gwf"]

 -- begin forM loop
 forM frameFileName $ \fileName -> do
  print fileName
  readData1 <- readFrame channelName (fileName)
  let getGpsTime = read $ (!!2) $ splitOn "-" $ last $ splitOn "/" fileName :: Double
  print $ (showFFloat (Just 0) getGpsTime "" )
  let dataChannel  = map realToFrac (eval readData1)

  -- spectrogram of seismic data
  let dataSpect = SU.gwspectrogram ifs ifs dfs dataChannel
  let fname =  (showFFloat (Just 0) getGpsTime "" ) ++ "_" ++ (channelName) ++ "_seis_spect.png"
  let channelName' = (channelName) ++ "__" ++ (showFFloat (Just 0) getGpsTime "" ) :: String
  PM3.spectrogram PMOP.LogYZ PMOP.COLZ " " channelName' fname dataSpect

  -- plot s(t)
  -- let xdata1  = map (/dfs) $ take (length dataChannel) [1,2..]
  --     plotdata1 = zip xdata1 (dataChannel)
  -- let fname =  (showFFloat (Just 0) getGpsTime "" ) ++ "_" ++ (channelName) ++ "_vt.png"
  -- let channelName' = (channelName) ++ "__" ++ (showFFloat (Just 0) getGpsTime "" ) :: String
  -- PM.plot PMOP.Linear PMOP.Line ("time[sec]","s(t)") channelName' fname plotdata1

  -- let dataSpecSeis = SU.gwpsd dataChannel (length dataChannel) dfs
  --     fname =  (showFFloat (Just 0) getGpsTime "" ) ++ "_" ++ (channelName) ++ "_seis_sf.png"
  --     channelName' = (channelName) ++ "__" ++ (showFFloat (Just 0) getGpsTime "" ) :: String
  -- PM.plot PMOP.LogXY PMOP.Line ("frequency [Hz]","spectrum of seismic") channelName' fname (take (16*ifs_harf) dataSpecSeis)

  -- plot lock flag
  --let channelName = "X1:CTR-LSC_TRX_OUT16"
  --let dfs = 16 ::Double
  -- let channelName = "X1:CTR-LSC_TRX_OUT_DQ"
  -- let dfs = 16384 ::Double
  -- readData2 <- readFrame channelName (fileName)
  -- let data2  = map realToFrac (eval readData2)
  --     xdata2 = map (/dfs) $ take (length data2) [1,2..]
  --     plotdata2 = zip xdata2 data2
  -- let fname =  (showFFloat (Just 0) getGpsTime "" ) ++ "_" ++ (channelName) ++ "_lock_flag.png"
  -- let channelName' = (channelName) ++ "__" ++ (showFFloat (Just 0) getGpsTime "" ) :: String
  -- PM.plot PMOP.Linear PMOP.Line ("time[sec]","lock flag") channelName' fname plotdata2


  -- plot non-calibrated GW channel in time domain
  let channelName = "X1:CTR-LSC_ETMX_OUT_DQ"
  let dfs = 16384 ::Double
  readData3 <- readFrame channelName (fileName)
  let data3  = map realToFrac (eval readData3)
      xdata3 = map (/dfs) $ take (length data3) [1,2..]
      plotdata3 = zip xdata3 data3
  -- let fname =  (showFFloat (Just 0) getGpsTime "" ) ++ "_" ++ (channelName) ++ "_GWchannel.png"
  -- let channelName' = (channelName) ++ "__" ++ (showFFloat (Just 0) getGpsTime "" ) :: String
  -- PM.plot PM.Linear PM.Line ("time[sec]","GW channel") channelName' fname plotdata3

  -- spectrogram of non-calibrated GW channel
  let dataSpect2 = SU.gwspectrogram ifs ifs dfs data3
  let fname =  (showFFloat (Just 0) getGpsTime "" ) ++ "_" ++ (channelName) ++ "_GWchannel_spect.png"
  let channelName' = (channelName) ++ "__" ++ (showFFloat (Just 0) getGpsTime "" ) :: String
  PM3.spectrogram PMOP.LogYZ PMOP.COLZ " " channelName' fname dataSpect2
  

  -- using FFT, spectrum of non-calibrated GW channel
  -- gwpsd :: [Double]-> Int -> Double -> [(Double, Double)]
  -- gwpsd dat nfft fs = gwpsdCore Welch dat nfft fs Hann
  --let dataSpecGW = SU.gwpsd data3 (length data3) dfs
  --    fname =  (showFFloat (Just 0) getGpsTime "" ) ++ "_" ++ (channelName) ++ "_GWchannel_sf.png"
  --    channelName' = (channelName) ++ "__" ++ (showFFloat (Just 0) getGpsTime "" ) :: String
  --PM.plot PMOP.LogXY PMOP.Line ("frequency [Hz]","interporated transfer function") channelName' fname dataSpecGW


  -- using transfer function, calculate calibrated spectrum
  --let dataSpecGW' = take (length trfDataf) dataSpecGW
  --    dataCalibSpect = zipWith (*) (map snd dataSpecGW') trfDataG
  --    dataCalibSpectSet = zip trfDataf dataCalibSpect
  --    fname =  (showFFloat (Just 0) getGpsTime "" ) ++ "_" ++ (channelName) ++ "_GWchannel_calibrated.png"
  --    channelName' = (channelName) ++ "__" ++ (showFFloat (Just 0) getGpsTime "" ) :: String
  --PM.plot PMOP.LogXY PMOP.Line ("frequency [Hz]","calibrated spectrum of GW channel") channelName' fname dataCalibSpectSet
  --PM.plot PMOP.Linear PMOP.Line ("frequency [Hz]","calibrated spectrum of GW channel") channelName' fname dataCalibSpectSet


  --print $ length dataCalibSpect
  --print $ length dataSpecGW'
  --print $ length trfDataf

  --print $ take 20 $ reverse trfDataf
  --print $ take 20 $ reverse dataCalibSpect

  -- spectrogram of calibrated GW channel 
  -- this need IFFT, so now its difficult.
  --let dataSpect3 = SU.gwspectrogram ifs_harf ifs_harf dfs 
  --let fname =  (showFFloat (Just 0) getGpsTime "" ) ++ "_" ++ (channelName) ++ "_GWchannel_spect.png"
  --PM3.spectrogram PMOP.LogYZ PMOP.COLZ " " channelName' fname dataSpect3

  --print $ take 15 $ zip (map fst dataSpecGW) trfDataf
  --print $ length (map fst dataSpecGW')
  --print $ length trfDataf
  --print $ length trfDataG
  --print $ head $ reverse (map fst dataSpecGW)

 -- end forM loop  
  


  

string2Int :: String -> Int
string2Int str = read str::Int

string2Double :: String -> Double
string2Double str = read str::Double

