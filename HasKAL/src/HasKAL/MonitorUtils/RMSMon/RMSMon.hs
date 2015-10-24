module HasKAL.MonitorUtils.RMSMon.RMSMon(
       rmsMon,
       refac_rmsMon
) where

import qualified Data.Vector.Generic as DVG
import qualified Numeric.LinearAlgebra as NLA
import Data.Maybe (fromMaybe, fromJust)
import Data.List (transpose)

import HasKAL.SpectrumUtils.SpectrumUtils (gwpsdV, gwOnesidedPSDVP)
import HasKAL.SpectrumUtils.Signature


{-- Expose Functions --}
refac_rmsMon :: Int -- ^ chunk size
             -> Double -- ^ sampling rate [Hz]
             -> NLA.Vector Double -- ^ analysis data in time series 
             -> [(Double, Double)] -- ^ frequency bands (f_low, f_high)
             -> [(NLA.Vector Double, NLA.Vector Double)] -- ^ (time, RMS value) in different frequency bands
refac_rmsMon nmon fs ys freq = do
 let nSplit    = (DVG.length ys) `div` nmon :: Int
     duration  = fromIntegral nmon / fs :: Double
     listindex = [0..nSplit-1]::[Int]
 let yslist = map (sliceys ys nmon) listindex :: [NLA.Vector Double]
        where sliceys :: NLA.Vector Double -> Int -> Int -> NLA.Vector Double
              sliceys ys nmon i = DVG.slice (nmon*i) nmon ys
 let rmslist = map (refac_rmsMoncore nmon duration fs freq) yslist :: [[Double]]
 let rmslist' = transpose rmslist :: [[Double]]
 -- rmslist  :: [[rms1(t1), rms1(t2)..], [rms2(t1), rms2(t2)..],[rms3(t1), rms3(t2)]]
 -- rmslist' :: [[rms1(t1), rms2(t1), rms3(t1)..], [rms1(t2), rms2(t2), rms3(t2)..],..]

 let tn         = (fromIntegral nSplit -1)*duration :: Double
     timevector = NLA.fromList [0, 1.0*duration..tn] :: NLA.Vector Double
     rmsoutput = map (formatrms timevector) rmslist' :: [(NLA.Vector Double, NLA.Vector Double)]
                 where formatrms :: NLA.Vector Double -> [Double] -> (NLA.Vector Double, NLA.Vector Double)
                       formatrms timevector rms = (timevector, NLA.fromList rms)
 rmsoutput
-- [(DVG.empty, DVG.empty)]


refac_rmsMoncore :: Int -- ^ chunk size
                 -> Double -- ^ duration [s]
                 -> Double -- ^ sampling rate [Hz]
                 -> [(Double, Double)] -- ^ frequency bands, [(f_low, f_high)]
                 -> NLA.Vector Double -- ^ chunk data in time series
                 -> [Double] -- ^ RMS value
refac_rmsMoncore nmon duration fs freq yschunk = do
                 let gwpsd = gwOnesidedPSDVP yschunk nmon fs
                     df = 1.0/duration :: Double 
                     freq' = filter (\(f1, f2) -> f1 > 0.0 && f2 > 0.0) freq
                 map sqrt $ map (*df) $ map (\(f1, f2) -> refac_sumHoff fs gwpsd f1 f2) freq'

{-- Internal Functions --}
refac_sumHoff::Double -- ^ sampling rate [Hz]
            -> (NLA.Vector Double, NLA.Vector Double) -- ^ power spectrum (f, s(f))
            -> Double -- ^ low frequency to calculate RMS
            -> Double -- ^ high frequency to calculate RMS
            -> Double -- ^ calculated RMS value
refac_sumHoff fs gwpsd f1 f2 = do
                 let indx1' = DVG.findIndex (>=(min f1 f2)) $ fst gwpsd
                     indx2' = DVG.findIndex (>(max f1 f2))  $ fst gwpsd
                     indx1  = fromJust indx1'
                     indx2  = fromJust indx2'
                 DVG.sum . DVG.slice indx1 (indx2 - indx1) $ snd gwpsd
                 -- If you used gwOnesidedPSDVP, factor 2 is not needed.
                 -- (*2) . DVG.sum . DVG.slice indx1 (indx2 - indx1) $ snd hoff
                 -- The output of gwpsdV is two-sided spectrum -> need factor 2











rmsMon :: Int -> Double -> NLA.Vector Double -> [(Double, Double)] -> [(NLA.Vector Double, NLA.Vector Double)]
rmsMon nmon fs ys freq = do
 let nSplit    = (DVG.length ys) `div` nmon :: Int
 let duration  = fromIntegral nmon / fs :: Double
 map (\(f1, f2) -> rmsMoncore nSplit duration fs ys f1 f2) freq
     where rmsMoncore :: Int -> Double -> Double -> NLA.Vector Double -> Double -> Double -> (NLA.Vector Double,NLA.Vector Double)
           rmsMoncore nSplit duration fs ys f1 f2
            | f1 <= 0.0 && f2 <= 0.0 = (DVG.empty, DVG.empty)
            | otherwise = do
               let df = 1.0/duration :: Double 
               let tn = (fromIntegral nSplit -1)*duration::Double
               let timevector = NLA.fromList [0, 1.0*duration..tn]::NLA.Vector Double
               let listindex = [0..nSplit-1]::[Int]
               let rmsvector = NLA.fromList $ map sqrt $ map (*df) $ map (sumHoff nSplit fs ys f1 f2) listindex
               (timevector, rmsvector)



{-- Internal Functions --}
sumHoff::Int -> Double -> NLA.Vector Double -> Double -> Double -> Int -> Double
sumHoff nSplit fs ys f1 f2 i = do
 let nchunk = (DVG.length ys) `div` nSplit ::Int
-- let hoff = gwpsdV (DVG.slice (nchunk*i) nchunk ys) nchunk fs
 let hoff = gwOnesidedPSDVP (DVG.slice (nchunk*i) nchunk ys) nchunk fs
 let indx1' = DVG.findIndex (>=(min f1 f2)) $ fst hoff
     indx2' = DVG.findIndex (>(max f1 f2))  $ fst hoff
     indx1  = fromJust indx1'
     indx2  = fromJust indx2'
 DVG.sum . DVG.slice indx1 (indx2 - indx1) $ snd hoff
 -- If you used gwOnesidedPSDVP, factor 2 is not needed.
 -- (*2) . DVG.sum . DVG.slice indx1 (indx2 - indx1) $ snd hoff
 -- The output of gwpsdV is two-sided spectrum -> need factor 2

