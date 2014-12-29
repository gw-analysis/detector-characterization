{-# HADDOCK Markdown #-}
{- |
Module      : HasKAL.MonitorUtils.RayleighMon.RayleighMon
Description : This is documentation tests.
Copyright   : (c) WhoAmI, 2014
License     : ???
Maintainer  : hoge@hoge.com
Stability   : test
Portability : POSIX

RayleighMonitor Function
-}

module HasKAL.MonitorUtils.RayleighMon.RayleighMon (
   rayleighMon
  ,rayleighMonV
) where

import Data.List (sort)
import System.IO.Unsafe (unsafePerformIO)
import Data.Vector.Unboxed as V
import Data.Matrix.Unboxed as M hiding ((!))
import qualified Control.Monad as CM (forM)

import HasKAL.Misc.UMatrixMapping
import HasKAL.SpectrumUtils.Signature
import HasKAL.SpectrumUtils.Function
import HasKAL.ExternalUtils.GSL.RandomNumberDistributions (gslCdfRayleighPinv)

{-- Expose Functions --}
-- | rayleigh monitor
rayleighMon :: [Double] -- ^ p values
            -> Double -- ^ sampling rate [Hz]
            -> Int -- ^ stride 
            -> Int -- ^ df
            -> [(Double, Double)] -- ^ averaged spectrum Sn(f)
            -> [(Double, Double, Double)] -- ^ spectrogram h(t, f)
            -> [([(Double, Double)], [(Double, Double)])]
rayleighMon pVals fsample stride fClust snf hfs = fromSpectrumPair $
  rayleighMonV pVals fsample stride fClust (toSpectrum snf) (toSpectrogram hfs)
    where fromSpectrumPair xs = unsafePerformIO $ CM.forM xs $ \(x,y) -> return $ (fromSpectrum x, fromSpectrum y)

-- | rayleigh monitor (vector I/O)
rayleighMonV :: [Double] -- ^ pValues
             -> Double -- ^ sampling rate [Hz]
             -> Int -- ^ stride
             -> Int -- ^ df
             -> Spectrum -- ^ averaged spectrum Sn(f)
             -> Spectrogram -- ^ spectrogram h(t, f)
             -> [(Spectrum, Spectrum)]
rayleighMonV pVals fsample stride fClust (freqV1, specV1) (tV2, freqV2, specM2) = do
  let snf' = V.map sqrt $ convert $ specV1
      hfs' = M.map ((*sqrt 2.0).sqrt) $ convertS2U $ specM2
      wMat = whiteningSpectrogram snf' hfs'
      df = fromIntegral fClust * fsample / fromIntegral stride
      newFV = fromList [df, df*2 .. df * fromIntegral ((V.length $ convert freqV2)`div`fClust)]
  unsafePerformIO $ CM.forM pVals $ \pVal -> do
        let noiseLv = rMonM pVal $ frequencyClusteringM fClust wMat
            theorem = gslCdfRayleighPinv pVal 1.0
            theoremV = (convert $ fromList [0.0, V.last newFV], convert $ fromList [theorem, theorem])
        return $ ((convert newFV, convert noiseLv), theoremV)

runningRayleighMonV :: Double -> Double -> Int -> Int -> Int -> Int -> Spectrum -> Spectrogram -> Spectrogram
runningRayleighMonV pVal fsample stride chunck shift fClust (freqV1, specV1) (tV2, freqV2, specM2) = do
  let snf' = V.map sqrt $ convert $ specV1
      hfs' = M.map ((*sqrt 2.0).sqrt) $ convertS2U $ specM2
      wMat = whiteningSpectrogram snf' hfs'
      dt = fromIntegral (shift*stride) / fsample
      df = fromIntegral fClust * fsample / fromIntegral stride
      newSpecM = fromColumns $ unsafePerformIO $ CM.forM [0, shift..cols wMat - chunck] $ \idx -> do
        return $ rMonM pVal $ frequencyClusteringM fClust $ subMatrix (0, rows wMat - 1) (idx, idx+chunck-1) wMat
      newTV = fromList [0, dt .. dt * fromIntegral (cols newSpecM - 1)]
      newFV = fromList [df, df*2 .. df * fromIntegral (rows newSpecM)]
  (convert newTV, convert newFV, convertU2S newSpecM)


{-- Internal Functions --}
rMonM :: Double -> Matrix Double -> Vector Double
rMonM pVal datM = mapRows0 (getEmpiricalQuantile pVal) datM

frequencyClusteringM :: Int -> Matrix Double -> Matrix Double
frequencyClusteringM num mat = fromVector newRow newCol $ slice 0 (newCol*newRow) $ flatten $ mat
  where newCol = num * oldCol
        newRow = oldRow*oldCol `div` newCol
        oldCol = cols mat
        oldRow = rows mat

whiteningSpectrogram :: Vector Double -> Matrix Double -> Matrix Double
whiteningSpectrogram snf hfs = mapCols1 whiteningSpectrum snf hfs

whiteningSpectrum :: Vector Double -> Vector Double -> Vector Double
whiteningSpectrum snf hf = V.zipWith (/) hf snf

getEmpiricalQuantile :: Double -> Vector Double -> Double
getEmpiricalQuantile pVal datV = V.head $ V.drop (pIdx-1) $ sort4Vec datV
  where pIdx = truncate $ pVal * (fromIntegral $ V.length datV)
        sort4Vec = fromList.sort.V.toList
