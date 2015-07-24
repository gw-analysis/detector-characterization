import Numeric.LinearAlgebra  (subVector)
import Numeric (showFFloat)
import Control.Monad (forM)
import System.Environment (getArgs)
import System.Random
import System.Process
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)

import HasKAL.FrameUtils.FrameUtils (readFrame)
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
   maybereadDataGW <- readFrame channelName fileName
   let readDataGW = fromMaybe (error " no data in the file.") maybereadDataGW
   let dataGW  = map realToFrac readDataGW


 ---------------------------------
 -- read seis channel
 ---------------------------------
   let channelName = "X1:CTR-PSL_SEIS_IN1_DQ" ::String
   let dfs = 2048.0 ::Double
   maybereadDataSEIS <- readFrame channelName fileName
   let readDataSEIS = fromMaybe (error " no data in the file.") maybereadDataSEIS
   let dataSEIS  = map realToFrac readDataSEIS


 ---------------------------------
 -- output text data
 ---------------------------------
   let dfs  = 2048.0 ::Double
       maxN = floor $ 0.01 * dfs ::Int

   let dataFilteredDownSample = skipListByK 8 0 dataGW

   
   let fname = (showFFloat (Just 0) dgetGpsTime "" ) ++ "__fs2048_SEIS_GW__RAW.txt" ::[Char]
   writeFile fname $ taple2string dataSEIS dataFilteredDownSample
   
   return ()
