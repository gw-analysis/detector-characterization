{-******************************************
  *     File Name: SRMon.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/12/04 18:10:19
  *******************************************-}

module SRMon (
   timeShift
  ,srMonM
) where

import Data.List (sort)
import System.IO.Unsafe (unsafePerformIO)
import Data.Vector.Unboxed as V
import Data.Matrix.Unboxed as M hiding ((!))
import qualified Control.Monad as CM (forM)

import MatrixSupplement

-- HasKAL
import qualified HasKAL.MonitorUtils.SRMon.StudentRayleighFunctions as SRF
import HasKAL.SpectrumUtils.Signature

timeShift :: (Matrix Double -> Vector Double) -> Double -> Int -> Int -> Int -> Int -> Spectrum -> Spectrogram -> Spectrogram
timeShift fx fsample stride chunck shift clusteringF (freqV1, specV1) (tV2, freqV2, specM2) = do
  let snf' = V.map sqrt $ convert $ specV1
      hfs' = M.map ((*sqrt 2.0).sqrt) $ convertS2U $ specM2
      wMat = whiteningSpectrogram snf' hfs'
      dt = fromIntegral (shift*stride) / fsample
      df = fromIntegral clusteringF * fsample / fromIntegral stride
      newSpecM = fromColumns $ unsafePerformIO $ CM.forM [0, shift..cols wMat - chunck] $ \idx -> do
        return $ fx $ frequencyClusteringM clusteringF $ subMatrix (0, rows wMat - 1) (idx, idx+chunck-1) wMat
      newTV = fromList [0, dt .. dt * fromIntegral (cols newSpecM - 1)]
      newFV = fromList [df, df*2 .. df * fromIntegral (rows newSpecM)]
  (convert newTV, convert newFV, convertU2S newSpecM)


frequencyClusteringM :: Int -> Matrix Double -> Matrix Double
frequencyClusteringM num mat = fromVector newRow newCol $ slice 0 (newCol*newRow) $ flatten $ mat
  where newCol = num * oldCol
        newRow = oldRow*oldCol `div` newCol
        oldCol = cols mat
        oldRow = rows mat

whiteningSpectrogram :: Vector Double -> Matrix Double -> Matrix Double
whiteningSpectrogram snf hfs = mapColumns1 whiteningSpectrum snf hfs

whiteningSpectrum :: Vector Double -> Vector Double -> Vector Double
whiteningSpectrum snf hf = V.zipWith (/) hf snf

-- Unbox function
srMonM :: Double -> Matrix Double -> Vector Double
srMonM pVal dataFs = mapRows0 (getNuV pVal) $ dataFs

getNuV :: Double -> Vector Double -> Double
getNuV pVal dataF = getClosestValueV e t
  where e = empiricalQuantileV pVal dataF
        t = V.map (theoreticalQuantileV pVal) $ fromList [2.0, 2.1..100.0]

empiricalQuantileV :: Double -> Vector Double -> Double
empiricalQuantileV pVal dataF = V.unsafeIndex (sort4Vec dataF) (pIdx -1)
  where pIdx = truncate $ pVal * (fromIntegral $ V.length dataF)
        sort4Vec = fromList.sort.V.toList -- vectorのソートに変更する

theoreticalQuantileV :: Double -> Double -> (Double, Double)
theoreticalQuantileV pVal nu = (SRF.hkalCdfStudentRayleighPinv sigma nu pVal, nu)
  where sigma = sqrt $ (nu - 2) / nu


getClosestValueV :: Double -> Vector (Double, Double) -> Double
getClosestValueV basis dat = snd $ V.minimum $ V.zip diff $ V.map snd dat
  where diff = V.map abs $ V.map ((-) basis) $ V.map fst dat

