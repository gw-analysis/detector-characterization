module HasKAL.MonitorUtils.RMSMon.RMSMon(
       rmsMon,
       rmsDailyMon
) where

import qualified Data.Vector.Generic as DVG
import qualified Numeric.LinearAlgebra as NLA
import Data.Maybe (fromMaybe, fromJust)

import HasKAL.SpectrumUtils.SpectrumUtils (gwpsdV)
import HasKAL.SpectrumUtils.Signature


{-- Expose Functions --}
rmsMon :: Int -> Double -> NLA.Vector Double -> [(Double, Double)] -> [(NLA.Vector Double, NLA.Vector Double)]
rmsMon nSplit fs ys freq = do
 let nduration = fromIntegral $ (DVG.length ys) `div` nSplit :: Double
 let duration  = nduration / fs :: Double
 map (\(f1, f2) -> rmsMoncore nSplit duration fs ys f1 f2) freq
     where rmsMoncore :: Int -> Double -> Double -> NLA.Vector Double -> Double -> Double -> (NLA.Vector Double,NLA.Vector Double)
           rmsMoncore nSplit duration fs ys f1 f2 = do
             let df = 1.0/duration :: Double 
             let tn = (fromIntegral nSplit -1)*duration::Double
             let timevector = NLA.fromList [0, duration..tn]::NLA.Vector Double
             let listindex = [0..nSplit-1]::[Int]
             let rmsvector = NLA.fromList $ map sqrt $ map (*df) $ map (sumHoff nSplit fs ys f1 f2) listindex
             (timevector, rmsvector)

rmsDailyMon :: Double -> NLA.Vector Double -> [(Double, Double)] -> [(NLA.Vector Double, NLA.Vector Double)]
rmsDailyMon fs ys freq = do
 let nSplit = 96::Int -- assuming data is 24hour and divide into 15 pieces => 96 chunks
 let duration  = 0.15::Double
 map (\(f1, f2) -> rmsDailyMoncore nSplit duration fs ys f1 f2) freq
     where rmsDailyMoncore :: Int -> Double -> Double -> NLA.Vector Double -> Double -> Double -> (NLA.Vector Double,NLA.Vector Double)
           rmsDailyMoncore nSplit duration fs ys f1 f2 = do
             let df = 1.0/duration :: Double 
             let tn = (fromIntegral nSplit -1) * duration::Double
             let timevector = NLA.fromList [0, duration..tn]::NLA.Vector Double
             let listindex = [0..nSplit-1]::[Int]
             let rmsvector = NLA.fromList $ map sqrt $ map (*df) $ map (sumHoff nSplit fs ys f1 f2) listindex
             (timevector, rmsvector)

{-- Internal Functions --}
sumHoff::Int -> Double -> NLA.Vector Double -> Double -> Double -> Int -> Double
sumHoff nSplit fs ys f1 f2 i = do
 let nchunk = (DVG.length ys) `div` nSplit ::Int
 let hoff = gwpsdV (DVG.slice (nchunk*i) nchunk ys) nchunk fs
 let indx1' = DVG.findIndex (>=(min f1 f2)) $ fst hoff
     indx2' = DVG.findIndex (>(max f1 f2)) $ fst hoff
     indx1  = fromJust indx1'
     indx2  = fromJust indx2'
 (*2) . DVG.sum . DVG.slice indx1 (indx2 - indx1) $ snd hoff
 -- output of gwpsdV is two-sided spectrum -> need factor 2

