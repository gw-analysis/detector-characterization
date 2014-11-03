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
     endGPS   = 7780 :: Int
     listGPS' = [startGPS,(startGPS+16)..endGPS]::[Int]
     listGPS  = map show listGPS'
 let frameFileName' = map ("/home/yuzurihara/frame/clio/X-R-103465"++) listGPS
     frameFileName  = map (++ "-16.gwf") frameFileName'
 -- --print frameFileName

 --let frameFileName = ["/home/yuzurihara/frame/clio/X-R-1034657664-16.gwf"]

 forM frameFileName $ \fileName -> do

   let getGpsTime = read $ (!!2) $ splitOn "-" $ last $ splitOn "/" fileName :: Int
   let dgetGpsTime = read $ (!!2) $ splitOn "-" $ last $ splitOn "/" fileName :: Double
   let channelName = "X1:CTR-PSL_SEIS_IN1_DQ"
   let dfs = 2048 ::Double
   readData1 <- readFrame channelName (fileName)
   let data1  = map realToFrac (eval readData1)
       tt1 = map (/dfs) $ take (length data1) [1,2..]


   let channelName = "X1:CTR-PSL_SEIS_IN1_DQ" ::String
   let dfs = 2048.0 ::Double
   readData2 <- readFrame channelName (fileName)
   let data2  = map realToFrac (eval readData2)
       tt = map (/dfs) $ take (length data2) [1,2..]

-- resampling from 16384Hz to 2048Hz
-- low-pass filter of cutoff frequency = 1024Hz
-- let dataLPF = butter 4 dfs (2048::Double) Low
   let n=4::Int
   let (numCoeffHigh,denomCoeffHigh) = butter n dfs 80.0 High :: ([Double], [Double])
       dataFiltered = iirFilter data2 numCoeffHigh denomCoeffHigh
 
   let (numCoeffLow,denomCoeffLow) = butter n dfs 300.0 Low :: ([Double], [Double])
       dataFiltered' = iirFilter dataFiltered numCoeffLow denomCoeffLow
       dataFiltered'' = take ((length dataFiltered') - 20) $ drop 10 dataFiltered'
--  let dfs  = 2048.0 ::Double
--      maxN = floor $ 0.01 * dfs ::Int

   --PM.plotX PMOP.Linear PMOP.Line ("time[sec]","s(t)") (show getGpsTime) $ zip tt dataFiltered''

   let ifs = 16384::Int
       dfs = 16384::Double
       ifs_harf = 8192::Int
       ifs_harf_harf = 4096::Int
       ifs_8 = 2048::Int
   let dataSpect = gwspectrogram ifs_8 ifs_harf dfs data2
   let fname =  (showFFloat (Just 0) dgetGpsTime "" ) ++ "_" ++ (channelName) ++ "_seis_spect_harf.png"
   --let fname = "hoge.png"
   let dataSpect' = filter (\x -> (snd' x) > 10.0) dataSpect
   PM3.spectrogram PMOP.LogYZ PMOP.COLZ " " (show getGpsTime) fname dataSpect'
