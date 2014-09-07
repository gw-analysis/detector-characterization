-- test code of HasKAL.MonitorUtils.CorrelationMon.CalCorrelation
-- read 1 frame data and for each channel data, calculate correlation value.

import Numeric.LinearAlgebra  --subVector
import Numeric --showFFloat
import Control.Monad -- forM
import System.Environment -- getArgs
import System.Process -- system
import Data.List.Split -- splitOn

import HasKAL.FrameUtils.FrameUtils -- read Frame file

import qualified HasKAL.PlotUtils.PlotOption.PlotOptionHROOT as PMOP
import qualified HasKAL.PlotUtils.HROOT.PlotGraph as PM
import qualified HasKAL.PlotUtils.HROOT.PlotGraph3D as PM3

import qualified HasKAL.SpectrumUtils.SpectrumUtils as SU

main = do

 let fileName = "/home/yuzurihara/frame/clio/X-R-1034657488-16.gwf"
 let getGpsTime = read $ (!!2) $ splitOn "-" $ last $ splitOn "/" fileName :: Double
 print $ (showFFloat (Just 0) getGpsTime "" )


 let channelName = "X1:CTR-LSC_ETMX_OUT_DQ"
 let dfs = 16384 ::Double
 let ifs = 16384 :: Int
     ifs_harf = floor $ (fromIntegral ifs)/2.0


 readData3 <- readFrame channelName (fileName)
 let data3  = map realToFrac (eval readData3)
     xdata3 = map (/dfs) $ take (length data3) [1,2..]
     plotdata3 = zip xdata3 data3

 -- spectrogram of non-calibrated GW channel
 let dataSpect2 = SU.gwspectrogram ifs ifs dfs data3
 let fname =  (showFFloat (Just 0) getGpsTime "" ) ++ "_" ++ (channelName) ++ "_GWchannel_spect_no_overlap.png"
 let channelName' = "gwspectrogram  fs fs  fs  data "
 PM3.spectrogram PMOP.LogYZ PMOP.COLZ " " channelName' fname dataSpect2

 let tt = map fst' dataSpect2 ::[Double]
     ff = map snd' dataSpect2 ::[Double]
     ss = map trd' dataSpect2 ::[Double]
 let fname = "data_spectrograme_no_overlap.txt"
 writeFile fname $ taple2string tt ff ss


 --print $ take 10 $ reverse dataSpect2

 -- spectrogram of non-calibrated GW channel
 let dataSpect2 = SU.gwspectrogram ifs_harf ifs dfs data3
 let fname =  (showFFloat (Just 0) getGpsTime "" ) ++ "_" ++ (channelName) ++ "_GWchannel_spect_overlap1.png"
 let channelName' = "gwspectrogram  fs/2 fs  fs  data "
 PM3.spectrogram PMOP.LogYZ PMOP.COLZ " " channelName' fname dataSpect2
 let tt = map fst' dataSpect2 ::[Double]
     ff = map snd' dataSpect2 ::[Double]
     ss = map trd' dataSpect2 ::[Double]
 let fname = "data_spectrograme_overlap_fs_harf.txt"
 writeFile fname $ taple2string tt ff ss



 -- spectrogram of non-calibrated GW channel
 let ifs_harf_harf = floor $ (fromIntegral ifs) /4.0
 let dataSpect2 = SU.gwspectrogram ifs_harf_harf ifs dfs data3
 let fname =  (showFFloat (Just 0) getGpsTime "" ) ++ "_" ++ (channelName) ++ "_GWchannel_spect_overlap2.png"
 let channelName' = "gwspectrogram  fs/4  fs  fs  data "
 PM3.spectrogram PMOP.LogYZ PMOP.COLZ " " channelName' fname dataSpect2
 let tt = map fst' dataSpect2 ::[Double]
     ff = map snd' dataSpect2 ::[Double]
     ss = map trd' dataSpect2 ::[Double]
 let fname = "data_spectrograme_overlap_fs_harf_harf.txt"
 writeFile fname $ taple2string tt ff ss
  


fst' :: (Double, Double, Double)  -> Double
fst' (a, b, c) = a

snd' :: (Double, Double, Double)  -> Double
snd' (a, b, c) = b

trd' :: (Double, Double, Double)  -> Double
trd' (a, b, c) = c

taple2string ::[Double] -> [Double] -> [Double]-> String
taple2string a b c = unlines  $ zipWith (++) (map show a) $ zipWith (++) (repeat " ") $ zipWith (++) (map show b) $ zipWith (++) (repeat " ")  $ zipWith (++) (repeat " ")  (map show c)

string2Int :: String -> Int
string2Int str = read str::Int

string2Double :: String -> Double
string2Double str = read str::Double

