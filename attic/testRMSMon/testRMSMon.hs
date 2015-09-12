import HasKAL.PlotUtils.HROOT.PlotGraph
import HasKAL.PlotUtils.HROOT.PlotGraph3D
import HasKAL.FrameUtils.Function (readFrameV)
--import HasKAL.SpectrumUtils.GwPsdMethod
import HasKAL.SpectrumUtils.SpectrumUtils (gwpsdV)
import HasKAL.SpectrumUtils.Signature

import qualified Data.Vector.Generic as DVG
import qualified Numeric.LinearAlgebra as NLA
import Data.Maybe (fromMaybe, fromJust)
--import System.Environment (getArgs)
import Control.Monad (forM)

import ReadFiles

--import qualified Data.Complex as DC
--import Data.Complex (Complex( (:+) ))


--import HasKAL.TimeUtils.GPSfunction
--import HasKAL.DataBaseUtils.Function (kagraDataGet)
-- error
--zs <- kagraDataGet 1124077565 64 "K1:PEM-EX_MAG_X_FLOOR"

main = do

 {-- open frame file --}
 let fs = 2048::Double
     ifs = 2048::Int
 let nfile = 100 :: Int -- you can change
     filelist = take nfile testFiles

 let totalduration  = 32 * nfile :: Int
     nSplit = 20 :: Int -- you can change
     iduration = totalduration `div` nSplit :: Int
     duration = fromIntegral iduration

-- let gps = 1124077533::Double
 let gpsstart = 77533::Double


 -- ToDdo: use mySQL seaver's function
 let channel  = "K1:PEM-EX_MAG_X_FLOOR"
 maybexs <- mapM (readFrameV channel) filelist
 let xs = map (fromMaybe (error " no data in the file.")) maybexs
 let ys = DVG.concat xs
 --print $ DVG.take 100 ys
 --print $ DVG.length ys


 let fname = "hoge.png"
 let color = [BLUE, RED, PINK]
 let freq  = [(0.1, 1.0), (1.0, 4.0), (4.0, 8.0)]::[(Double, Double)]
 let gpsend = gpsstart+(fromIntegral nSplit -1)*duration::Double
 let rms   = calculateRMS nSplit gpsstart duration fs ys freq
 oPlotV Linear LinePoint 1 color ("GPS[sec]", "V/s^2") 0.05 "RMSMon" fname ((gpsstart,gpsend),(0.01,0.25)) rms
 
 return 0



calculateRMS :: Int -> Double -> Double -> Double -> NLA.Vector Double -> [(Double, Double)] -> [(NLA.Vector Double, NLA.Vector Double)]
calculateRMS nSplit gpsstart duration fs ys freq = 
 map (\(f1, f2) -> calculateRMScore nSplit gpsstart duration fs ys f1 f2) freq

calculateRMScore :: Int -> Double -> Double -> Double -> NLA.Vector Double -> Double -> Double -> (NLA.Vector Double,NLA.Vector Double)
calculateRMScore nSplit gpsstart duration fs ys f1 f2 = do
 let listindex = [0..nSplit-1]::[Int]
 let rmsvector = NLA.fromList $ map (sumHoff nSplit fs ys f1 f2) listindex
 let gpsend = gpsstart+(fromIntegral nSplit -1)*duration::Double
 let gpsvector = NLA.fromList [gpsstart, gpsstart+duration..gpsend]::NLA.Vector Double
 (gpsvector, rmsvector)

sumHoff::Int -> Double -> NLA.Vector Double -> Double -> Double -> Int -> Double
sumHoff nSplit fs ys f1 f2 i = do
 let nchunk = (DVG.length ys) `div` nSplit ::Int
 let hoff = gwpsdV (DVG.slice (nchunk*i) nchunk ys) nchunk fs
 let indx1' = DVG.findIndex (>=(min f1 f2)) $ fst hoff
     indx2' = DVG.findIndex (>(max f1 f2)) $ fst hoff
     indx1  = fromJust indx1'
     indx2  = fromJust indx2'
 DVG.sum . DVG.slice indx1 (indx2 - indx1) $ snd hoff


