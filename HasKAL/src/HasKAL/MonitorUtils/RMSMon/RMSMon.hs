module HasKAL.MonitorUtils.RMSMon.RMSMon(
       rmsMon,
       rmsMonWaveData
) where

import qualified Data.Vector.Generic as DVG
import qualified Numeric.LinearAlgebra as NLA
import Data.Maybe (fromMaybe, fromJust)
import Data.List (transpose, partition)

import HasKAL.SpectrumUtils.SpectrumUtils (gwpsdV, gwOnesidedPSDVP)
import HasKAL.SpectrumUtils.Signature
import HasKAL.WaveUtils.Data (WaveData(..))


{-- Expose Functions --}
rmsMon :: Int -- ^ chunk size
             -> Double -- ^ sampling rate [Hz]
             -> NLA.Vector Double -- ^ analysis data in time series 
             -> [(Double, Double)] -- ^ frequency bands (f_low, f_high)
             -> [Spectrum] -- ^ (time, RMS value) in different frequency bands
rmsMon nmon fs ys freq = do
 let nSplit    = (DVG.length ys) `div` nmon :: Int
     duration  = fromIntegral nmon / fs :: Double
     listindex = [0..nSplit-1]::[Int]
 let yslist = map (sliceys ys nmon) listindex :: [NLA.Vector Double]
        where sliceys :: NLA.Vector Double -> Int -> Int -> NLA.Vector Double
              sliceys ys nmon i = DVG.slice (nmon*i) nmon ys
 let rmslist = map (rmsMoncore nmon duration fs freq) yslist :: [[Double]]
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

rmsMonWaveData :: Int -- ^ chunk size
                   -> [(Double, Double)] -- ^ frequency bands (f_low, f_high)
                   -> WaveData -- ^ the data to calculate RMS
                   -> [Spectrum] -- ^ (time, RMS value) in different frequency bands
rmsMonWaveData nmon freq st = rmsMon nmon fs ys freq
  where fs = samplingFrequency st
        ys = gwdata st


{-- Internal Functions --}
rmsMoncore :: Int -- ^ chunk size
                 -> Double -- ^ duration [s]
                 -> Double -- ^ sampling rate [Hz]
                 -> [(Double, Double)] -- ^ frequency bands, [(f_low, f_high)]
                 -> NLA.Vector Double -- ^ chunk data in time series
                 -> [Double] -- ^ RMS value
rmsMoncore nmon duration fs freq yschunk = do
--                 let gwpsd = gwOnesidedPSDVP yschunk nmon fs
                 let gwpsd = gwpsdV yschunk nmon fs
                     df = 1.0/duration :: Double 
                     -- remove the elements of zero
                     freq1 = snd $ partition (\(f1, f2) -> f1==0.0 && f2==0.0) freq
                     -- remove the elements of negative
                     freq2 = snd $ partition (\(f1, f2) -> f1<0.0 || f2<0.0) freq1
                     -- remove the larger elements than Nyquist frequency
                     freq3 = snd $ partition (\(f1, f2) -> f1>(fs/2.0) || f2>(fs/2.0) ) freq2
                 map sqrt $ map (*df) $ map (\(f1, f2) -> sumHoff fs gwpsd f1 f2) freq3


sumHoff::Double -- ^ sampling rate [Hz]
            -> (NLA.Vector Double, NLA.Vector Double) -- ^ power spectrum (f, s(f))
            -> Double -- ^ low frequency to calculate RMS
            -> Double -- ^ high frequency to calculate RMS
            -> Double -- ^ calculated RMS value
sumHoff fs gwpsd f1 f2 = do
                 let indx1' = DVG.findIndex (>=(min f1 f2)) $ fst gwpsd
                     indx2' = DVG.findIndex (>(max f1 f2))  $ fst gwpsd
                     indx1  = fromJust indx1'
                     indx2  = fromJust indx2'
                 --DVG.sum . DVG.slice indx1 (indx2 - indx1) $ snd gwpsd
                 -- If you used gwOnesidedPSDVP, factor 2 is not needed.
                 (*2) . DVG.sum . DVG.slice indx1 (indx2 - indx1) $ snd gwpsd
                 -- The output of gwpsdV is two-sided spectrum -> need factor 2

