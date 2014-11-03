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
   readData2 <- readFrame channelName (fileName)
   let data2  = map realToFrac (eval readData2)
       tt = map (/dfs) $ take (length data2) [1,2..]

 ---------------------------------
 -- Band Pass Filter
 --------------------------------- 
   let n=4::Int
       flow  = 10::Double
       fhigh = 106::Double
   let (numCoeffHigh,denomCoeffHigh) = butter n dfs flow High :: ([Double], [Double])
       dataFiltered = iirFilter data2 numCoeffHigh denomCoeffHigh
 
   let (numCoeffLow,denomCoeffLow) = butter n dfs fhigh Low :: ([Double], [Double])
       dataFiltered' = iirFilter dataFiltered numCoeffLow denomCoeffLow
--       dataFiltered'' = take ((length dataFiltered') - n*8) $ drop (n*4) dataFiltered'
--       tt'            = take ((length dataFiltered') - n*8) $ drop (n*4) tt
       dataFiltered'' = drop (10000) dataFiltered'
       tt'            = drop (10000) tt
   --PM.plotX PMOP.Linear PMOP.Line ("time[sec]","gw channel after BPF") (show getGpsTime) $ zip tt' dataFiltered''
   --PM.plotX PMOP.Linear PMOP.Line ("time[sec]","gw channel after BPF") (show getGpsTime) $ zip tt data2

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
   let dataSpect = gwspectrogram (div ifs_8 2) ifs_8 dfs dataFiltered''
   --let dataSpect = gwspectrogram (div ifs_harf_harf 2) ifs_harf_harf dfs dataFiltered''
   let fcut1 = 10.0::Double
       fcut2 = 60.0::Double
       dataSpect' = filter (\x -> (snd' x) > fcut1 && (snd' x) < fcut2) dataSpect
       fname = (showFFloat (Just 0) dgetGpsTime "" ) ++ "_" ++ (channelName) ++ "_gw_BPFiltered_" ++(show ifs_8) ++ "_" ++ (show fcut1) ++"_"++ (show fcut2) ++ ".png"
   PM3.spectrogram PMOP.LogYZ PMOP.COLZ " " (show getGpsTime) fname dataSpect'



 ---------------------------------
 -- seis channel
 ---------------------------------
   let getGpsTime = read $ (!!2) $ splitOn "-" $ last $ splitOn "/" fileName :: Int
   let dgetGpsTime = read $ (!!2) $ splitOn "-" $ last $ splitOn "/" fileName :: Double

   let channelName = "X1:CTR-PSL_SEIS_IN1_DQ" ::String
   let dfs = 2048.0 ::Double
   readData2 <- readFrame channelName (fileName)
   let data2  = map realToFrac (eval readData2)
       tt = map (/dfs) $ take (length data2) [1,2..]

 ---------------------------------
 -- Band Pass Filter
 --------------------------------- 
   let n=8::Int
       flow = 80::Double
       fhigh = 300::Double
   let (numCoeffHigh,denomCoeffHigh) = butter n dfs flow High :: ([Double], [Double])
       dataFiltered = iirFilter data2 numCoeffHigh denomCoeffHigh
   let (numCoeffLow,denomCoeffLow) = butter n dfs fhigh Low :: ([Double], [Double])
       dataFiltered' = iirFilter dataFiltered numCoeffLow denomCoeffLow
       dataFiltered'' = take ((length dataFiltered') - n*8) $ drop (n*4) dataFiltered'
       tt'            = take ((length dataFiltered') - n*8) $ drop (n*4) tt
   --PM.plotX PMOP.Linear PMOP.Line ("time[sec]","seis channel after BPF") (show getGpsTime) $ zip tt' dataFiltered''
   --PM.plotX PMOP.Linear PMOP.Line ("time[sec]","seis channel after BPF") (show getGpsTime) $ zip tt data2


 ---------------------------------
 -- seis channel  spectrogram
 ---------------------------------
   let ifs = 2048::Int
       dfs = 2048::Double
       ifs_harf = 1024::Int
       ifs_harf_harf = 512::Int
       ifs_8 = 256::Int
   let dataSpect = gwspectrogram (div ifs_harf_harf 2) ifs_harf_harf dfs dataFiltered''
   let fcut1 = 10.0::Double
       fcut2 = 600::Double
       dataSpect' = filter (\x -> (snd' x) > fcut1 && (snd' x) < fcut2) dataSpect
       fname =  (showFFloat (Just 0) dgetGpsTime "" ) ++ "_" ++ (channelName) ++ "_seis_BPFiltered_" ++(show ifs_8) ++ "_" ++ (show fcut1) ++"_"++ (show fcut2) ++ ".png"
   PM3.spectrogram PMOP.LogYZ PMOP.COLZ " " (show getGpsTime) fname dataSpect'
