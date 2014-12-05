{-******************************************
  *     File Name: SRMon.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/12/05 16:55:02
  *******************************************-}

-- Reference
---- [1] C.Rover, Phys. Rev. D 84, 122004 (2011)

-- Test code
---- detector-characterization/attic/SRMon_refactoring/test.hs

module SRMon (
   FitMethod(LSM, MLE, QUANT)
  ,studentRayleighMon
  ,studentRayleighMonV
) where

import Data.List (sort)
import System.IO.Unsafe (unsafePerformIO)
import Data.Vector.Unboxed as V
import Data.Matrix.Unboxed as M hiding ((!))
import qualified Control.Monad as CM (forM)

import HasKAL.Misc.UMatrixMapping
import HasKAL.MonitorUtils.SRMon.Signature
import HasKAL.MonitorUtils.SRMon.StudentRayleighFunctions
import HasKAL.SpectrumUtils.Signature
import HasKAL.SpectrumUtils.Function

studentRayleighMon :: FitMethod -> Double -> Int -> Int -> Int -> Int -> 
  [(Double, Double)] -> [(Double, Double, Double)] -> [(Double, Double, Double)]
studentRayleighMon method fsample stride chunck shift clusteringF snf hfs = plotFormatedSpectogram $
  studentRayleighMonV method fsample stride chunck shift clusteringF (toSpectrum snf) (toSpectrogram hfs)

studentRayleighMonV :: FitMethod -> Double -> Int -> Int -> Int -> Int -> Spectrum -> Spectrogram -> Spectrogram
studentRayleighMonV method fsample stride chunck shift clusteringF (freqV1, specV1) (tV2, freqV2, specM2) = do
  let snf' = V.map sqrt $ convert $ specV1
      hfs' = M.map ((*sqrt 2.0).sqrt) $ convertS2U $ specM2
      wMat = whiteningSpectrogram snf' hfs'
      dt = fromIntegral (shift*stride) / fsample
      df = fromIntegral clusteringF * fsample / fromIntegral stride
      newSpecM = fromColumns $ unsafePerformIO $ CM.forM [0, shift..cols wMat - chunck] $ \idx -> do
        return $ srMonM method $ frequencyClusteringM clusteringF $ subMatrix (0, rows wMat - 1) (idx, idx+chunck-1) wMat
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
whiteningSpectrogram snf hfs = mapCols1 whiteningSpectrum snf hfs

whiteningSpectrum :: Vector Double -> Vector Double -> Vector Double
whiteningSpectrum snf hf = V.zipWith (/) hf snf

-- Unbox function
srMonM :: FitMethod -> Matrix Double -> Vector Double
srMonM method dataFs
  | method == LSM = error "Not Implemented yet."
  | method == MLE = error "Not Implemented yet."
  | method == (QUANT pVal) = mapRows0 (getNuQuantV pVal) dataFs
  where (QUANT pVal) = method


getNuQuantV :: Double -> Vector Double -> Double
getNuQuantV pVal dataF = getClosestValueV e t
  where e = empiricalQuantileV pVal dataF
        t = V.map (theoreticalQuantileV pVal) $ fromList [2.0, 2.1..100.0]

empiricalQuantileV :: Double -> Vector Double -> Double
empiricalQuantileV pVal dataF = V.unsafeIndex (sort4Vec dataF) (pIdx -1)
  where pIdx = truncate $ pVal * (fromIntegral $ V.length dataF)
        sort4Vec = fromList.sort.V.toList -- vectorのソートに変更する

theoreticalQuantileV :: Double -> Double -> (Double, Double)
theoreticalQuantileV pVal nu = (hkalCdfStudentRayleighPinv sigma nu pVal, nu)
  where sigma = sqrt $ (nu - 2) / nu


getClosestValueV :: Double -> Vector (Double, Double) -> Double
getClosestValueV basis dat = snd $ V.minimum $ V.zip diff $ V.map snd dat
  where diff = V.map abs $ V.map ((-) basis) $ V.map fst dat

