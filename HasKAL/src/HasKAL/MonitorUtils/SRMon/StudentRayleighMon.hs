


module HasKAL.MonitorUtils.SRMon.StudentRayleighMon (
   FitMethod(LSM, MLE, QUANT)
  ,studentRayleighMon
  ,studentRayleighMonV
) where

import Data.List (sort)
import System.IO.Unsafe (unsafePerformIO)
import Data.Vector.Unboxed as V
import Data.Matrix.Unboxed as M hiding ((!), convert)
import qualified Control.Monad as CM (forM)
import qualified Data.Vector.Algorithms.Heap as H (sort)

import HasKAL.Misc.UMatrixMapping
import HasKAL.MonitorUtils.SRMon.Signature
import HasKAL.MonitorUtils.SRMon.StudentRayleighFunctions
import HasKAL.SpectrumUtils.Signature
import HasKAL.SpectrumUtils.Function

{-- Expose Functions --}
studentRayleighMon :: FitMethod
                   -> Double -- ^ sampling rate [Hz]
                   -> Int -- ^ stride
                   -> Int -- ^ chunck size
                   -> Int -- ^ time shift
                   -> Int -- ^ df
                   -> [(Double, Double)] -- ^ averaged spectrum Sn(f)
                   -> [(Double, Double, Double)] -- ^ spectrogram h(t, f)
                   -> [(Double, Double, Double)] -- ^ nu(t, f)
studentRayleighMon method fsample stride chunck shift clusteringF snf hfs = plotFormatedSpectogram $
  studentRayleighMonV method fsample stride chunck shift clusteringF (toSpectrum snf) (toSpectrogram hfs)

studentRayleighMonV :: FitMethod 
                    -> Double -- ^ sampling rate [Hz]
                    -> Int -- ^ stride
                    -> Int -- ^ chunck size
                    -> Int -- ^ time shift
                    -> Int -- ^ df
                    -> Spectrum -- ^ averaged spectrum Sn(f)
                    -> Spectrogram -- ^ h(t, f)
                    -> Spectrogram -- ^ nu(t, f)
studentRayleighMonV method fsample stride chunck shift clusteringF (freqV1, specV1) (tV2, freqV2, specM2) = do
  let snf' = V.map sqrt $ convert $ specV1
      hfs' = M.map ((*sqrt 2.0).sqrt) $ convertS2U $ specM2
      wMat = whiteningSpectrogram snf' hfs'
      dt = fromIntegral (shift*stride) / fsample
      df = fromIntegral clusteringF * fsample / fromIntegral stride
  let newSpecM = fromColumns $ unsafePerformIO $ CM.forM [0, shift..cols wMat - chunck] $ \idx -> do
        let subM = subMatrix (0, idx) (rows wMat - 1, idx+chunck-1) wMat
        return $ srMonM method $ frequencyClusteringM clusteringF $ subM
      newTV = fromList [0, dt .. dt * fromIntegral (cols newSpecM - 1)]
      newFV = fromList [df, df*2 .. df * fromIntegral (rows newSpecM)]
  (convert newTV, convert newFV, convertU2S newSpecM)


{-- Internal Functions --}
srMonM :: FitMethod -> Matrix Double -> Vector Double
srMonM method dataFs
  | method == LSM = error "Not Implemented yet."
  | method == MLE = error "Not Implemented yet."
  | method == (QUANT pVal) = mapRows0 (getNuQuantV pVal) dataFs
  where (QUANT pVal) = method

frequencyClusteringM :: Int -> Matrix Double -> Matrix Double
frequencyClusteringM num mat = fromVector (newRow, newCol) $ slice 0 (newCol*newRow) $ flatten $ mat
  where newCol = num * oldCol
        newRow = oldRow*oldCol `div` newCol
        oldCol = cols mat
        oldRow = rows mat

whiteningSpectrogram :: Vector Double -> Matrix Double -> Matrix Double
whiteningSpectrogram snf hfs = mapCols1 whiteningSpectrum snf hfs

whiteningSpectrum :: Vector Double -> Vector Double -> Vector Double
whiteningSpectrum snf hf = V.zipWith (/) hf snf


----  Quantile Base Estimator
getNuQuantV :: Double -> Vector Double -> Double
getNuQuantV pVal dataF = getClosestValueV e t
  where e = empiricalQuantileV pVal dataF
        t = V.map (theoreticalQuantileV pVal) $ fromList [2.0, 2.1..200.0]

empiricalQuantileV :: Double -> Vector Double -> Double
empiricalQuantileV pVal dataF = V.unsafeIndex (sort4Vec dataF) (pIdx -1)
  where pIdx = truncate $ pVal * (fromIntegral $ V.length dataF)
        -- sort4Vec = fromList.sort.V.toList -- vectorのソートに変更する
        sort4Vec = modify H.sort

theoreticalQuantileV :: Double -> Double -> (Double, Double)
theoreticalQuantileV pVal nu = (hkalCdfStudentRayleighPinv sigma nu pVal, nu)
  where sigma = sqrt $ (nu - 2) / nu


getClosestValueV :: Double -> Vector (Double, Double) -> Double
getClosestValueV basis dat = snd $ V.minimum $ V.zip diff $ V.map snd dat
  where diff = V.map abs $ V.map ((-) basis) $ V.map fst dat

