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

 let startGPS = 7664 :: Int
     endGPS   = 7680 :: Int
     listGPS' = [startGPS,(startGPS+16)..endGPS]::[Int]
     listGPS  = map show listGPS'
 let frameFileName' = map ("/home/yuzurihara/frame/clio/X-R-103465"++) listGPS
     frameFileName  = map (++ "-16.gwf") frameFileName'
 -- --print frameFileName

-- let frameFileName = ["/home/yuzurihara/frame/clio/X-R-1034657664-16.gwf"]

 let iDropbNumberGW = 10000::Int
     iDropbNumberSEIS = 2500::Int

 print iDropbNumberGW
 print iDropbNumberSEIS

 forM frameFileName $ \fileName -> do

   let getGpsTime = read $ (!!2) $ splitOn "-" $ last $ splitOn "/" fileName :: Int
   let dgetGpsTime = read $ (!!2) $ splitOn "-" $ last $ splitOn "/" fileName :: Double

 ---------------------------------
 -- GW channel
 ---------------------------------
   let channelName = "X1:CTR-LSC_ETMX_OUT_DQ"
   let ifs = 16384::Int
       dfs = 16384::Double
       ifs_harf = 8192::Int
       ifs_harf_harf = 4096::Int
       ifs_8 = 2048::Int
       ifs_16 = 1024::Int
   readDataGW <- readFrame channelName (fileName)
   let dataGW  = map realToFrac (eval readDataGW)
       ttGW = map (/dfs) $ take (length dataGW) [1,2..]

 ---------------------------------
 -- Band Pass Filter
 --------------------------------- 
   let n=4::Int
       flow  = 10::Double
       fhigh = 60::Double
   let (numCoeffHigh,denomCoeffHigh) = butter n dfs flow High :: ([Double], [Double])
       dataGWFiltered = iirFilter dataGW numCoeffHigh denomCoeffHigh
 
   let (numCoeffLow,denomCoeffLow) = butter n dfs fhigh Low :: ([Double], [Double])
       dataGWFiltered' = iirFilter dataGWFiltered numCoeffLow denomCoeffLow
       dataGWFiltered'' = drop (iDropbNumberGW) dataGWFiltered'
       ttGW'            = drop (iDropbNumberGW) ttGW

   print $ length ttGW'
   print $ length dataGWFiltered''
   

   PM.plotX PMOP.Linear PMOP.Line ("time[sec]","gw channel after BPF") (show getGpsTime) $ zip ttGW' dataGWFiltered''
   --print dataFiltered'


   ---------------------------------
   -- GW channel   compare BPF ari/nashi
   ---------------------------------   
--   let data3 = gwpsd data2 ifs dfs
--       data4 = gwpsd dataFiltered'' (length dataFiltered'') (realToFrac $ length dataFiltered'')
--       data5 = filter (\x -> (fst x) < realToFrac ifs_harf ) data4
   --PM.oPlotX PMOP.LogXY PMOP.Line ("f [Hz]", "GW channel v(f)") [(show getGpsTime)]  [data3, data5]

   --print $ map snd data3
   -- cat hoge.txt | tr "," "\n" | tr -d "(" | tr -d ")" | tr -d "]" | tr -d "[" > t_data3.txt

   ---------------------------------
   -- GW channel   spectrogram
   ---------------------------------   
   let dataGWSpect = gwspectrogram (div ifs_8 2) ifs_8 dfs dataGWFiltered''
   --let dataSpect = gwspectrogram (div ifs_harf_harf 2) ifs_harf_harf dfs dataFiltered''
   let fcut1 = 10.0::Double
       fcut2 = 60.0::Double
       dataGWSpect' = filter (\x -> (snd' x) > fcut1 && (snd' x) < fcut2) dataGWSpect
       fname = (showFFloat (Just 0) dgetGpsTime "" ) ++ "_" ++ (channelName) ++ "_gw_BPFiltered_" ++(show ifs_8) ++ "_" ++ (show fcut1) ++"_"++ (show fcut2) ++ ".png"
   PM3.spectrogram PMOP.LogYZ PMOP.COLZ " " (show getGpsTime) fname dataGWSpect'





 ---------------------------------
 -- seis channel
 ---------------------------------
   let getGpsTime = read $ (!!2) $ splitOn "-" $ last $ splitOn "/" fileName :: Int
   let dgetGpsTime = read $ (!!2) $ splitOn "-" $ last $ splitOn "/" fileName :: Double

   let channelName = "X1:CTR-PSL_SEIS_IN1_DQ" ::String
   let dfs = 2048.0 ::Double
   readDataSEIS <- readFrame channelName (fileName)
   let dataSEIS  = map realToFrac (eval readDataSEIS)
       ttSEIS = map (/dfs) $ take (length dataSEIS) [1,2..]



 ---------------------------------
 -- Band Pass Filter
 --------------------------------- 
   let n=8::Int
       flow = 80::Double
       fhigh = 300::Double
   let (numCoeffHigh,denomCoeffHigh) = butter n dfs flow High :: ([Double], [Double])
       dataSEISFiltered = iirFilter dataSEIS numCoeffHigh denomCoeffHigh
   let (numCoeffLow,denomCoeffLow) = butter n dfs fhigh Low :: ([Double], [Double])
       dataSEISFiltered' = iirFilter dataSEISFiltered numCoeffLow denomCoeffLow
       dataSEISFiltered'' = drop (iDropbNumberSEIS) dataSEISFiltered'
       ttSEIS'            = drop (iDropbNumberSEIS) ttSEIS

   
   print $ length ttSEIS'
   print $ length dataSEISFiltered''
   PM.plotX PMOP.Linear PMOP.Line ("time[sec]","seis channel after BPF") (show getGpsTime) $ zip ttSEIS dataSEISFiltered''


 ---------------------------------
 -- seis channel  spectrogram
 ---------------------------------
   let ifs = 2048::Int
       dfs = 2048::Double
       ifs_harf = 1024::Int
       ifs_harf_harf = 512::Int
       ifs_8 = 256::Int
   let dataSEISSpect = gwspectrogram (div ifs_harf_harf 2) ifs_harf_harf dfs dataSEISFiltered''
   let fcut1 = 10.0::Double
       fcut2 = 600::Double
       dataSEISSpect' = filter (\x -> (snd' x) > fcut1 && (snd' x) < fcut2) dataSEISSpect
       fname =  (showFFloat (Just 0) dgetGpsTime "" ) ++ "_" ++ (channelName) ++ "_seis_BPFiltered_" ++(show ifs_8) ++ "_" ++ (show fcut1) ++"_"++ (show fcut2) ++ ".png"
   PM3.spectrogram PMOP.LogYZ PMOP.COLZ " " (show getGpsTime) fname dataSEISSpect'





 -- ---------------------------------
 -- -- output text data
 -- ---------------------------------
 --   let dfs  = 2048.0 ::Double
 --       maxN = floor $ 0.01 * dfs ::Int
 --   --print maxN

 --   result <- forM [0,1..7] $ \rest ->do
 --    let dataFilteredDownSample = skipListByK 8 rest dataGWFiltered
 --    -- calculate correlation value
 --    --takeCorrelation method x y maxN = case method of
 --    --let rValueLst = takeCorrelation Peason data1 dataFilteredDownSample maxN
 --    let rValueLst = takeCorrelation Peason dataGWFiltered dataFilteredDownSample maxN
 --    let rValue = maximum rValueLst
 --    --let rValue_10 = read (showFFloat (Just 10) rValue "")::Double
 --    let maxValueIndex = correlationResult2pickupMaxValueIndex rValueLst dfs
 --    let tapleCorrelationValue = (getGpsTime, rValue, rest + (fst maxValueIndex),(fromIntegral rest)/dfs + (snd maxValueIndex))
 --    --print $ tapleCorrelationValue
 --    return tapleCorrelationValue

 --   --print result 
 --   print $ maximum result
