import Numeric.LinearAlgebra  --subVector
import Numeric --showFFloat
import Control.Monad -- forM
import System.Environment -- getArgs
import System.Random
import System.Process -- system
import Data.List.Split -- splitOn

import HasKAL.FrameUtils.FrameUtils -- read Frame file
import HasKAL.SpectrumUtils.GwPsdMethod
import HasKAL.SpectrumUtils.SpectrumUtils
import HasKAL.SignalProcessingUtils.Filter
import HasKAL.SignalProcessingUtils.ButterWorth
import HasKAL.SignalProcessingUtils.FilterType

import UsefulFunction

main = do

 ---------------------------------
 ---------------------------------
 -- which frame file you read ?    
 -- let startGPS = 7664 :: Int
 --     endGPS   = 7680 :: Int
 ---------------------------------
 ---------------------------------

 --let listGPS' = [startGPS,(startGPS+16)..endGPS]::[Int]
 let listGPS' = [7664, 7680, 7616, 7648]::[Int]
     listGPS  = map show listGPS'
     frameFileName' = map ("/home/yuzurihara/frame/clio/X-R-103465"++) listGPS
     frameFileName  = map (++ "-16.gwf") frameFileName'
-- let frameFileName = ["/home/yuzurihara/frame/clio/X-R-1034657664-16.gwf"]



 ---------------------------------
 -- number of samples to drop to care IIR filtered data
 ---------------------------------
 let iDropbNumberGW = 2000::Int
     iDropbNumberSEIS = 500::Int

 print iDropbNumberGW
 print iDropbNumberSEIS


 forM frameFileName $ \fileName -> do
   let getGpsTime = read $ (!!2) $ splitOn "-" $ last $ splitOn "/" fileName :: Int
   let dgetGpsTime = read $ (!!2) $ splitOn "-" $ last $ splitOn "/" fileName :: Double

 ---------------------------------
 -- read GW channel
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

 ---------------------------------
 -- GW channel -> Band Pass Filter
 -- How filter you want ?
   let n=4::Int
       flow  = 100::Double
       fhigh = 200::Double
 --------------------------------- 
   let (numCoeffHigh,denomCoeffHigh) = butter n dfs flow High :: ([Double], [Double])
       dataGWFiltered = iirFilter dataGW numCoeffHigh denomCoeffHigh
       dataGWFiltered' = drop (iDropbNumberGW) dataGWFiltered
 
   let (numCoeffLow,denomCoeffLow) = butter n dfs fhigh Low :: ([Double], [Double])
       dataGWFiltered'' = iirFilter dataGWFiltered' numCoeffLow denomCoeffLow
       dataGWFiltered''' = drop (iDropbNumberGW) dataGWFiltered''

   


 ---------------------------------
 -- read seis channel
 ---------------------------------
   let channelName = "X1:CTR-PSL_SEIS_IN1_DQ" ::String
   let dfs = 2048.0 ::Double
   readDataSEIS <- readFrame channelName (fileName)
   let dataSEIS  = map realToFrac (eval readDataSEIS)


 ---------------------------------
 -- seis channel -> Band Pass Filter
 -- How filter you want ?
   let n=8::Int
       flow  = 10::Double
       fhigh = 200::Double
 --------------------------------- 

   let (numCoeffHigh,denomCoeffHigh) = butter n dfs flow High :: ([Double], [Double])
       dataSEISFiltered = iirFilter dataSEIS numCoeffHigh denomCoeffHigh
       dataSEISFiltered' = drop (iDropbNumberSEIS) dataSEISFiltered

   let (numCoeffLow,denomCoeffLow) = butter n dfs fhigh Low :: ([Double], [Double])
       dataSEISFiltered'' = iirFilter dataSEISFiltered' numCoeffLow denomCoeffLow
       dataSEISFiltered''' = drop (iDropbNumberSEIS) dataSEISFiltered''



 ---------------------------------
 -- output text data
 ---------------------------------
   let dfs  = 2048.0 ::Double
       maxN = floor $ 0.01 * dfs ::Int

   let dataFilteredDownSample = skipListByK 8 0 dataGWFiltered'''
   
   let fname = (showFFloat (Just 0) dgetGpsTime "" ) ++ "__fs2048__SEIS_BPF_100_200_vs_GW_BPF_100_200.txt" ::[Char]
   writeFile fname $ taple2string dataSEISFiltered''' dataFilteredDownSample
   
   return ()
