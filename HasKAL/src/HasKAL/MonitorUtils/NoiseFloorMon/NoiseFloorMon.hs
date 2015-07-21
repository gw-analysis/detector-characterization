module HasKAL.MonitorUtils.NoiseFloorMon.NoiseFloorMon
( 
estimateThreshold, 
getNoiseFloorStatus
)
where


import qualified HasKAL.ExternalUtils.GSL.RandomNumberGeneration as RNG
import qualified HasKAL.ExternalUtils.GSL.RandomNumberDistributions as RND
--import Data.Vector.Storable as VS
import qualified HasKAL.Misc.StrictMapping as HMS

import HasKAL.FrameUtils.FrameUtils
--import SpectrumUtilsNoiseMon
--import LinearPredictionNoiseMon
import HasKAL.SpectrumUtils.SpectrumUtils
import HasKAL.SignalProcessingUtils.LinearPrediction
import HasKAL.SignalProcessingUtils.Resampling

{- For plotting -}
--import HasKAL.PlotUtils.PlotOption.PlotOptionHROOT
--import qualified HasKAL.PlotUtils.HROOT.PlotGraph as HR

{- For fft -}
import Numeric.GSL.Fourier
import Numeric.LinearAlgebra

{- For low-high pass filter-}
import HasKAL.SignalProcessingUtils.Filter
import HasKAL.SignalProcessingUtils.ButterWorth

import HROOT hiding (eval)

{-estimateThreshold should be done before applying getNoiseFloorStatus-}


estimateThreshold :: Int->Double->Double->Int->IO(Double,Double)
estimateThreshold tsSF minfreqband maxfreqband lcsz = do
   simnoise <- genTimeGauss lcsz
   noisemon <- getThresholdStatus simnoise tsSF minfreqband maxfreqband
   let rmnoise = map snd noisemon
       nfmmean' = sumV rmnoise
       nfmmean = (nfmmean' / (realToFrac (Prelude.length rmnoise)))
       nfmdev' = sumV  (Prelude.map (\x->(x-nfmmean)*(x-nfmmean)) rmnoise)
       nfmdev =  sqrt(nfmdev' / (realToFrac((Prelude.length rmnoise) - 1)))
   return $ (nfmmean,nfmdev)                         

{-
getNoiseFloorStatus
param1 : [Double] : time domain which want to check noise floor status
param2 : Int      : Sampling frequency of time domain data
param3 : Double   : minimum of frequency band to check noise floor status
param4 : Double   : maximum of frequency band to check noise floor status
param5 : [(Double,Double)] : (time, significance)
param6 : (Double,Double) : output of estimateThreshold
time :: set time=0 for first point of time domain data
significance :: deviation from standard Gaussian-Stationary noise floor
-}

getNoiseFloorStatus :: [Double]->Int->Double->Double->(Double,Double)->IO[(Double,Double)]
getNoiseFloorStatus ts tsSF minfreqband maxfreqband (nfmmean,nfmdev) = do
-- Hitomazu kokodeha let de initial parameter wo teigi suru
  let whitenFltOrder = 100 :: Int
      whitenFltSize = 10*tsSF :: Int
      rmSize = 256 :: Int -- 1/8 * 2048
      lpfOrder = 6 :: Int 
      hpfOrder = 6 :: Int
      nfmFltDelay = nfmCalcFltDelay tsSF whitenFltOrder lpfOrder hpfOrder minfreqband maxfreqband ::Int
      lcsz = length ts
      rmwz = rmSize
  --(nfmmean,nfmdev) <- estimateThreshold lcsz rmwz
  --print (nfmmean,nfmdev)
  whitendatsample <- getWhitensample ts tsSF whitenFltSize
  let psdmedian = gwpsd whitendatsample rmSize (fromIntegral tsSF)
      wtts =  whitening (lpefCoeff whitenFltOrder psdmedian) ts :: [Double]
  bpts <- applybandpass wtts tsSF minfreqband maxfreqband lpfOrder hpfOrder 
  --mapM_ print whitendatsample
  let bptssig = drop nfmFltDelay bpts :: [Double]
      bptssig2 = map (\x -> x*x) bptssig
      datrunmed = runmed bptssig rmSize
      datrunmedsig = map (\x -> abs ((x-nfmmean)/nfmdev)) datrunmed
      intervalrunmed = (fromIntegral rmSize) / (fromIntegral tsSF) :: Double
      trunmed = take (length datrunmed) [(intervalrunmed),(intervalrunmed*2)..]
  return $ zip trunmed datrunmedsig

getThresholdStatus :: [Double]->Int->Double->Double->IO[(Double,Double)]
getThresholdStatus ts tsSF minfreqband maxfreqband = do
-- Hitomazu kokodeha let de initial parameter wo teigi suru
  let whitenFltOrder = 100 :: Int
      whitenFltSize = 10*tsSF :: Int
      rmSize = 256 :: Int -- 1/8 * 2048
      lpfOrder = 6 :: Int 
      hpfOrder = 6 :: Int
      nfmFltDelay = nfmCalcFltDelay tsSF whitenFltOrder lpfOrder hpfOrder minfreqband maxfreqband ::Int
      lcsz = length ts
      rmwz = rmSize
  --(nfmmean,nfmdev) <- estimateThreshold lcsz rmwz
  --print (nfmmean,nfmdev)
  whitendatsample <- getWhitensample ts tsSF whitenFltSize
  let psdmedian = gwpsd whitendatsample rmSize (fromIntegral tsSF)
      wtts =  whitening (lpefCoeff whitenFltOrder psdmedian) ts :: [Double]
  bpts <- applybandpass wtts tsSF minfreqband maxfreqband lpfOrder hpfOrder 
  --mapM_ print whitendatsample
  let bptssig = drop nfmFltDelay bpts :: [Double]
      bptssig2 = map (\x -> x*x) bptssig
      datrunmed = runmed bptssig rmSize
      --datrunmedsig = map (\x -> abs ((x-nfmmean)/nfmdev)) datrunmed
      intervalrunmed = (fromIntegral rmSize) / (fromIntegral tsSF) :: Double
      trunmed = take (length datrunmed) [(intervalrunmed),(intervalrunmed*2)..]
  return $ zip trunmed datrunmed


applybandpass :: [Double]->Int->Double->Double->Int->Int->IO[Double]
applybandpass wtts tsSF minfreqband maxfreqband lpfOrder hpfOrder = do
  let (nfmNumCoeffLow, nfmDenomCoeffLow) = butter 6 (fromIntegral tsSF) maxfreqband Low
      (nfmNumCoeffHigh, nfmDenomCoeffHigh) = butter 6 (fromIntegral tsSF) minfreqband High
      nyquistfreq = (fromIntegral tsSF) / 2.0 :: Double
      bpts' = if (maxfreqband < nyquistfreq) then (iirFilter wtts nfmNumCoeffLow nfmDenomCoeffLow)
      	      	 	      	      	     else wtts
      bpts = if (minfreqband > 0.0) then (iirFilter bpts' nfmNumCoeffHigh nfmDenomCoeffHigh)
      	      	 	      	    else bpts'
  return bpts

nfmCalcFltDelay :: Int->Int->Int->Int->Double->Double->Int
nfmCalcFltDelay whitenFltOrder tsSF lpfOrder hpfOrder minfreqband maxfreqband = nfmFltDelay
  where nyquistfreq = (fromIntegral tsSF) / 2.0 :: Double
        whitenFltDelay =  whitenFltOrder :: Int
        lowpassFltDelay = if (maxfreqband < nyquistfreq) then lpfOrder :: Int
			     		    	         else 0 :: Int
	highpassFltDelay = if (minfreqband > 0.0) then hpfOrder :: Int
			      		     	  else 0 :: Int
	nfmFltDelay = (whitenFltDelay+lowpassFltDelay+highpassFltDelay) ::Int

getWhitensample :: [Double]->Int->Int->IO[Double]
getWhitensample ts tsSf whitenFltSize = do
  if ((length ts) < whitenFltSize) then (return $ take whitenFltSize [0,0..])
     			           else (return $ take whitenFltSize ts)



genTimeGauss :: Int->IO[Double]
genTimeGauss lcsz = do
   let simnoise = Prelude.take lcsz [1,1..] ::[Double]
   rng <- RNG.newRngWithSeed (-1)
   HMS.forM' simnoise $ \j -> do
     gaus <- RND.gslRanGaussian rng 1.0 
     return $ gaus


sumV :: [Double]->Double
sumV [] = 0.0
sumV (x:xs) = x + (sumV xs)

estimateThreshold' :: Int->Int->IO(Double,Double)
estimateThreshold' lcsz rmwz = do
   simnoise <- genTimeGauss lcsz
   let simnoise2 = Prelude.map (\x->x*x) simnoise
       datrunmed = runmed simnoise2 rmwz
       nfmmean' = sumV datrunmed
       nfmmean = sqrt (nfmmean' / (realToFrac (Prelude.length datrunmed)))
       nfmdev' = sumV (Prelude.map (\x->(x-nfmmean)*(x-nfmmean)) datrunmed)
       nfmdev = sqrt(nfmdev' / (realToFrac((Prelude.length datrunmed) - 1)))
   return $ (nfmmean,nfmdev)                         
