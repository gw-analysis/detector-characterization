{-# OPTIONS_GHC -XBangPatterns #-}

module NoiseFloorMon
( 
  NFMParam (..) 
, estimateThreshold 
, getNoiseFloorStatus
, defaultNFMparam
, makeNFMparam
)
where


import qualified HasKAL.ExternalUtils.GSL.RandomNumberGeneration as RNG
import qualified HasKAL.ExternalUtils.GSL.RandomNumberDistributions as RND
import qualified HasKAL.Misc.StrictMapping as HMS

import HasKAL.FrameUtils.FrameUtils
import HasKAL.SpectrumUtils.SpectrumUtils
import HasKAL.SignalProcessingUtils.LinearPrediction
import HasKAL.SignalProcessingUtils.Resampling

import Data.List (sort, foldl')

import Numeric.GSL.Fourier
import Numeric.LinearAlgebra
import qualified Data.Vector.Generic as G

import HasKAL.SignalProcessingUtils.Filter
import HasKAL.SignalProcessingUtils.ButterWorth

import HasKAL.WaveUtils.Data hiding (mean)
import HasKAL.WaveUtils.Signature
import HasKAL.TimeUtils.Signature
import HasKAL.DetectorUtils.Detector

--import HROOT hiding (eval, mean)

data NFMParam = NFMParam{ 
  tsreSF :: Double  
, whitenFltOrder :: Int
, whitenFltSize :: Int
, whitenFltSTime :: Int
, rmSize :: Int
, minfreq :: Double
, maxfreq :: Double 
, lpfOrder :: Int 
, hpfOrder :: Int
} deriving (Show, Eq, Read)

defaultNFMparam :: NFMParam
defaultNFMparam = NFMParam{
   tsreSF = 512.0 
,  whitenFltOrder = 100
,  whitenFltSize = 512 * 10
,  whitenFltSTime = 512 * 5
,  rmSize = 128 
,  minfreq = 64.0
,  maxfreq = 128.0
,  lpfOrder = 6
,  hpfOrder = 6
}

makeNFMparam :: Double->Int->Int->Int->Int->Double->Double->Int->Int->NFMParam
makeNFMparam rsf wfo wfs wfst rms minf maxf lpfo hpfo = 
   NFMParam{  
   	      tsreSF = rsf
   	   ,  whitenFltOrder = wfo
   	   ,  whitenFltSize = wfs
	   ,  whitenFltSTime = wfst
	   ,  rmSize = rms
	   ,  minfreq = minf
	   ,  maxfreq = maxf
	   ,  lpfOrder = lpfo
	   ,  hpfOrder = hpfo
   }

{-estimateThreshold should be done before applying getNoiseFloorStatus-}

estimateThreshold :: NFMParam->Int->IO(Double,Double)
estimateThreshold np lcsz = do
   simnoise <- genTimeGauss lcsz
   noisemon <- getThresholdStatus simnoise np
   let nfmmean' = sum noisemon
       nfmmean = (nfmmean' / (realToFrac (Prelude.length noisemon)))
       nfmdev' = sum  (Prelude.map (\x->(x-nfmmean)*(x-nfmmean)) noisemon)
       nfmdev =  sqrt(nfmdev' / (realToFrac((Prelude.length noisemon) - 1)))
   return $ (nfmmean,nfmdev)                         

getNoiseFloorStatus :: [Double]->Double->NFMParam->(Double,Double)->IO[(Double,Double)]
getNoiseFloorStatus ts tsSF np (nfmmean,nfmdev) = do
  let dsts = downsampleV tsSF (tsreSF np) (fromList ts)
      nfmFltDelay = nfmCalcFltDelay (whitenFltOrder np) (tsreSF np) (lpfOrder np) (hpfOrder np) (minfreq np) (maxfreq np) ::Int
  whitendatsample <- getWhitensample dsts np
  let psdmedian = gwpsdV whitendatsample (floor (tsreSF np)) (tsreSF np)
      startGPST = (0,0)::(Int, Int)
      stopGPST = (0,0)::(Int,Int)
      dstsWD = mkWaveData KAGRA "test" (tsreSF np) startGPST stopGPST dsts
--      wtts =  whitening (lpefCoeffV (whitenFltOrder np) psdmedian) (toList dsts) 
      wtts = whiteningWaveData (lpefCoeffV (whitenFltOrder np) psdmedian) dstsWD
  bpts <- applybandpass (gwdata wtts) np
  print "aaa"
  let ndatV = dim bpts
      bptssig = subVector nfmFltDelay (ndatV -nfmFltDelay) $ bpts
      bptssig2 = G.map (\x -> (x*x-nfmmean)/nfmdev) bptssig 
      datrunmed = runmedV bptssig2 (rmSize np)
      intervalrunmed = (fromIntegral (rmSize np)) / (tsreSF np) :: Double
      trunmed = [(intervalrunmed),(intervalrunmed*2)..]

--  print $ (lpefCoeffV (whitenFltOrder np) psdmedian)
--  print $ vjoin [bpts, fromList [-1.0,-1.0..]] 
--  print $ subVector nfmFltDelay (ndatV -nfmFltDelay) bpts
  return $ zip trunmed datrunmed
--  return $ take 1 $ zip [1..] wtts
--  return $ 

getThresholdStatus :: [Double]->NFMParam->IO[Double]
getThresholdStatus ts np = do
  let nfmFltDelay = nfmCalcFltDelay 0 (tsreSF np) (lpfOrder np) (hpfOrder np) (minfreq np) (maxfreq np) ::Int
  bpts <- applybandpass (fromList ts) np  
  let ndatV = dim bpts
      bptssig = subVector nfmFltDelay (ndatV -nfmFltDelay) $ bpts
      bptssig2 = G.map (\x -> x*x) bptssig 
      datrunmed = runmedV bptssig2 (rmSize np)
  return datrunmed

applybandpass :: Vector Double->NFMParam->IO(Vector Double)
applybandpass wtts np = do
  let nfmCoeffLow = butter (lpfOrder np) (tsreSF np) (maxfreq np) Low
      nfmCoeffHigh = butter (lpfOrder np) (tsreSF np) (minfreq np) High
      nyquistfreq = (tsreSF np) / 2.0 :: Double
      bpts' = if ((maxfreq np) < nyquistfreq) then (iir nfmCoeffLow wtts)
      	      	 	      	      	      else wtts
      bpts = if ((minfreq np) > 0.0) then (iir nfmCoeffHigh bpts')
      	      	 	      	     else bpts'
  return bpts

nfmCalcFltDelay :: Int->Double->Int->Int->Double->Double->Int
nfmCalcFltDelay whitenFltOrder tsSF lpfOrder hpfOrder minfreqband maxfreqband = nfmFltDelay
  where nyquistfreq = tsSF / 2.0 :: Double
        whitenFltDelay =  whitenFltOrder :: Int
        lowpassFltDelay = if (maxfreqband < nyquistfreq) then lpfOrder :: Int
			     		    	         else 0 :: Int
	highpassFltDelay = if (minfreqband > 0.0) then hpfOrder :: Int
			      		     	  else 0 :: Int
	nfmFltDelay = (whitenFltDelay+lowpassFltDelay+highpassFltDelay) ::Int

getWhitensample :: Vector Double->NFMParam->IO(Vector Double)
getWhitensample ts np = do
  if ((dim ts) < (whitenFltSize np)) 
     then (return $ fromList $ take (whitenFltSize np) [0,0..])
     else (return $ subVector (whitenFltSTime np) (whitenFltSize np) ts)



genTimeGauss :: Int->IO[Double]
genTimeGauss lcsz = do
   let simnoise = Prelude.take lcsz [1,1..] ::[Double]
   rng <- RNG.newRngWithSeed (-1)
   HMS.forM' simnoise $ \j -> do
     gaus <- RND.gslRanGaussian rng 1.0 
     return $ gaus


runmedV :: Vector Double -> Int -> [Double]
runmedV datV nmed = do 
  let ndat = dim datV
      maxitr = floor $ fromIntegral (ndat) / fromIntegral (nmed) :: Int
      datList = takesV  (take maxitr (repeat nmed)) datV :: [Vector Double]
  map median $ map (sort . toList) datList

mean :: Floating a => [a] -> a
mean x = fst $ foldl' (\(!m, !n) x -> (m+(x-m)/(n+1),n+1)) (0,0) x

median :: (Floating a, Ord a) => [a] -> a
median x | odd n  = head  $ drop (n `div` 2) x'
         | even n = mean $ take 2 $ drop i x'
                  where i = (length x' `div` 2) - 1
                        x' = sort x
                        n  = length x

runmed :: [Double] -> Int -> [Double]
runmed dat nmed = do 
  let datV = fromList dat
      ndat = dim datV
      maxitr = floor $ fromIntegral (ndat) / fromIntegral (nmed) :: Int
      datList = takesV  (take maxitr (repeat nmed)) datV :: [Vector Double]
  map median $ map (sort . toList) datList


