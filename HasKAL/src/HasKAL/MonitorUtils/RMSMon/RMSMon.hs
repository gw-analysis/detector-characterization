module HasKAL.MonitorUtils.RMSMon.RMSMon(
       rmsMon
) where

import qualified Data.Vector.Generic as DVG
import qualified Numeric.LinearAlgebra as NLA
import Data.Maybe (fromMaybe, fromJust)

import HasKAL.SpectrumUtils.SpectrumUtils (gwpsdV)
import HasKAL.SpectrumUtils.Signature


{-- Expose Functions --}
rmsMon :: Int -> Double -> Double -> Double -> NLA.Vector Double -> [(Double, Double)] -> [(NLA.Vector Double, NLA.Vector Double)]
rmsMon nSplit gpsstart duration fs ys freq = 
 map (\(f1, f2) -> rmsMoncore nSplit gpsstart duration fs ys f1 f2) freq


{-- Internal Functions --}
rmsMoncore :: Int -> Double -> Double -> Double -> NLA.Vector Double -> Double -> Double -> (NLA.Vector Double,NLA.Vector Double)
rmsMoncore nSplit gpsstart duration fs ys f1 f2 = do
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

