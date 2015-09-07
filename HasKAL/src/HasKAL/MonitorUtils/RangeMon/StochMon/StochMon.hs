module HasKAL.MonitorUtils.RangeMon.StochMon.StochMon(
  StochMonParam (..)
, makeStochMonParam
, h2omega_param 
, h2omega_adet
       ) where

import System.IO
import System.IO.Unsafe
import HasKAL.DetectorUtils.Detector
import Control.Concurrent
import HasKAL.ExternalUtils.GSL.RandomNumberDistributions
import HasKAL.SignalProcessingUtils.Interpolation
import Numeric.LinearAlgebra
import HasKAL.SpectrumUtils.DetectorSensitivity


data StochMonParam = StochMonParam{
  main_det :: Detector -- main detector 
, obs_time :: Double -- total observation time
, part_det :: Detector -- partner detector
, psd_maindet :: [(Double, Double)] --PSD of main detector
, min_freq :: Double -- minimum frequency to calculate
, max_freq :: Double -- maximum frequency to calculate
, res_freq :: Double -- frequency resolution to calculate
, sens_far :: Double -- Set False Alarm Ratio for sensitivity calculation 
, sens_dp :: Double -- Set Detection Probability for sensitivity calculation
}

makeStochMonParam :: Detector->Double->Detector->[(Double, Double)]->Double->Double->Double->Double->Double->StochMonParam
makeStochMonParam md ot pd psd minf maxf resf far dp = 
  StochMonParam{
    main_det = md
,   obs_time = ot
,   part_det = pd
,   psd_maindet = psd
,   min_freq = minf
,   max_freq = maxf
,   res_freq = resf
,   sens_far = far
,   sens_dp = dp
}

h2omega_adet :: StochMonParam->[(Double, Double)]
h2omega_adet smp = smresult
  where freqlist = [((min_freq smp)+0.0*(res_freq smp)), ((min_freq smp)+1.0*(res_freq smp))..(max_freq smp)]
        smfactor = (h2omega_factor (obs_time smp)) *  (h2omega_erfc smp)
        smpsd' = h2omega_psd_adet smp freqlist
        smpsd = map (**(-0.5)) smpsd'
        smresult = zip freqlist (map (*smfactor) smpsd)

h2omega_param :: StochMonParam->[(Double, Double)]
h2omega_param smp = smresult
  where freqlist = [((min_freq smp)+0.0*(res_freq smp)), ((min_freq smp)+1.0*(res_freq smp))..(max_freq smp)]
        smfactor = (h2omega_factor (obs_time smp)) *  (h2omega_erfc smp)
        smpsd' = h2omega_psd smp freqlist
        smpsd = map (**(-0.5)) smpsd'
        smresult = zip freqlist (map (*smfactor) smpsd)

h2omega_factor :: Double->Double
h2omega_factor ttot = 1/(sqrt(ttot))*10.0*(pi**2)/3.0/((3.2*1.0E-18)**2)*(sqrt(2.0))

h2omega_erfc :: StochMonParam->Double
h2omega_erfc smp = (erfc_inv $ 2.0*(sens_far smp))-(erfc_inv $ 2.0*(sens_dp smp))

--Function for error function
erfc_inv ::Double->Double
erfc_inv par = gslCdfGaussianQinv (par/2.0) sigma
  where sigma = 1/sqrt(2.0)
      
h2omega_psd :: StochMonParam->[Double]->[Double]
h2omega_psd smp freq = out_psd
  where orffactor2 = orf_detectors smp freq
        fin6 = map (**6) freq
        maindet_psd = interpV (map fst (psd_maindet smp)) (map snd (psd_maindet smp)) freq Linear
        partdet_psd = toList $ ifonoisepsd (part_det smp) (fromList freq)
        out_psd = calclist orffactor2 fin6 maindet_psd partdet_psd

h2omega_psd_adet :: StochMonParam->[Double]->[Double]
h2omega_psd_adet smp freq = out_psd
  where orffactor2 = orf_detectors smp freq
        fin6 = map (**6) freq
        maindet_psd = toList $ ifonoisepsd (main_det smp) (fromList freq)
        partdet_psd = toList $ ifonoisepsd (part_det smp) (fromList freq)
        out_psd = calclist orffactor2 fin6 maindet_psd partdet_psd


calclist :: [Double]->[Double]->[Double]->[Double]->[Double]
calclist [] _ _ _ = []
calclist _ [] _ _ = []
calclist _ _ [] _ = []
calclist _ _ _ [] = []
calclist (aa:as) (bb:bs) (cc:cs) (dd:ds) = (aa/bb/cc/dd) : calclist as bs cs ds 

orf_detectors :: StochMonParam->[Double]->[Double]
orf_detectors smp freq = freqord2
  where det_a = (main_det smp)
        det_b = (part_det smp)
        detid | ((det_a == (LIGO_Livingston)) && (det_b == (LIGO_Hanford))) || ((det_a == (LIGO_Hanford)) && (det_b == (LIGO_Livingston))) = 1
              | ((det_a == (LIGO_Livingston)) && (det_b == VIRGO))          || ((det_a == VIRGO)          && (det_b == (LIGO_Livingston))) = 2
              | ((det_a == (LIGO_Livingston)) && (det_b == KAGRA))          || ((det_a == KAGRA)          && (det_b == (LIGO_Livingston))) = 3
              | ((det_a == (LIGO_Hanford))    && (det_b == VIRGO))          || ((det_a == VIRGO)          && (det_b == (LIGO_Hanford)))    = 4
              | ((det_a == (LIGO_Hanford))    && (det_b == KAGRA))          || ((det_a == KAGRA)          && (det_b == (LIGO_Hanford)))    = 5
              | ((det_a == VIRGO)             && (det_b == KAGRA))          || ((det_a == KAGRA)          && (det_b == VIRGO))             = 6
              | otherwise                                                                                                                  = 1 --temporary
        xdata = [0.0..1000.0]
        ydata = giveorf detid
        freqord = interpV xdata ydata freq Linear
        freqord2 = map (**2) freqord

giveorf :: Int -> [Double]
giveorf id = map (!! id) read_ORF

readMultiColumn :: String -> [[Double]]
readMultiColumn = map (map read.words).lines

read_ORF :: [[Double]]
read_ORF = readMultiColumn (unsafePerformIO $ readFile "HasKAL/MonitorUtils/RangeMon/StochMon/orf_data_1.txt")

