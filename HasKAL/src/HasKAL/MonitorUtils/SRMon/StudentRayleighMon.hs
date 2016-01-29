


module HasKAL.MonitorUtils.SRMon.StudentRayleighMon (
   FitMethod(LSM, MLE, QUANT)
  ,studentRayleighMon
  ,studentRayleighMonWaveData
  ,studentRayleighMonV
  ,srMonNuHist
) where

import Prelude as P
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Packed.Vector as PV (subVector, fromList, toList, dim)
import qualified Data.Packed.Matrix as PM (cols, rows, toRows, fromColumns)
import Data.Vector.Unboxed as V
import Data.Matrix.Unboxed as M hiding ((!), convert)
import qualified Control.Monad as CM (forM)
import qualified Data.Vector.Algorithms.Heap as H (sort,select)

import HasKAL.Misc.UMatrixMapping
import HasKAL.MonitorUtils.SRMon.Signature
import HasKAL.MonitorUtils.SRMon.StudentRayleighFunctions
import HasKAL.SpectrumUtils.Signature
import HasKAL.SpectrumUtils.Function
import HasKAL.SpectrumUtils.SpectrumUtils (gwOnesidedPSDV, gwspectrogramV, gwOnesidedPSDWaveData, gwspectrogramWaveData)
import HasKAL.WaveUtils.Data (WaveData(..))

{-- Expose Functions --}
studentRayleighMon :: Double -> Double -> Double -> Double -> Double -> Double -> [Double] -> [Double] -> [(Double, Double, Double)]
studentRayleighMon quantile fs fftSec chunkT shiftT df snt ht
  = plotFormatedSpectogram $ studentRayleighMonV (QUANT quantile) fs nfft (t2c chunkT) (t2c shiftT) ndf snf hfs
  where nfft = truncate (fftSec * fs)
        ndf = truncate (df * fftSec)
        snf = gwOnesidedPSDV (PV.fromList snt) nfft fs
        hfs = gwspectrogramV 0 nfft fs $ PV.fromList ht
        t2c x = truncate (x / fftSec)

studentRayleighMonWaveData :: Double -> Double -> Double -> Double -> Double -> WaveData -> WaveData -> Spectrogram
studentRayleighMonWaveData quantile fftSec chunkT shiftT df snt ht
  = studentRayleighMonV (QUANT quantile) fs nfft (t2c chunkT) (t2c shiftT) ndf snf hfs
  where fs = samplingFrequency snt
        nfft = truncate (fftSec * fs)
        ndf = truncate (df * fftSec)
        snf = gwOnesidedPSDWaveData fftSec snt
        hfs = gwspectrogramWaveData 0 fftSec ht
        t2c x = truncate (x / fftSec)

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
  let snf' = V.map (sqrt.(*0.5)) $ convert $ specV1
      hfs' = M.map sqrt $ convertS2U $ specM2
      wMat = whiteningSpectrogram snf' hfs'
      dt = fromIntegral (shift*stride) / fsample
      df = fromIntegral clusteringF * fsample / fromIntegral stride
  let newSpecM = fromColumns $ unsafePerformIO $ CM.forM [0, shift..cols wMat - chunck] $ \idx -> do
        let subM = subMatrix (0, idx) (rows wMat - 1, idx+chunck-1) wMat
        return $ srMonM method $ frequencyClusteringM clusteringF $ subM
      newTV = fromList [0, dt .. dt * fromIntegral (cols newSpecM - 1)]
      newFV = fromList [df, df*2 .. df * fromIntegral (rows newSpecM)]
  (convert newTV, convert newFV, convertU2S newSpecM)

srMonNuHist :: Spectrogram -- ^ nu(t, f)    : output of studentRayleighMonV
            -> Spectrogram -- ^ yield(f, nu): for histogram plot
srMonNuHist (tV, fV, nuM) = (fV, PV.fromList $ P.init bins, yield)
  where bins = [1.0,2..201]
        yield = PM.fromColumns . P.map (PV.fromList . snd . histogram1d 0 0 bins . PV.toList) . PM.toRows $ nuM
        histogram1d xmin xmax bins input =
          let intervals = P.zipWith (\x y ->(x, y)) (P.init bins) (P.tail bins)
              within u x = x >= fst u && x < snd u
           in (P.map fst intervals, P.map ((fromIntegral.P.length) . (\u -> P.filter (within u) input)) intervals)



{-- Internal Functions --}
srMonM :: FitMethod -> Matrix Double -> Vector Double
srMonM method dataFs
  | method == LSM = error "Not Implemented yet."
  | method == MLE = error "Not Implemented yet."
  | method == (QUANT pVal) = mapRows0 (getNuQuantV pVal) dataFs
  -- | method == (QUANT pVal) = mapRows0 (empiricalQuantileV pVal) dataFs -- for test
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
empiricalQuantileV pVal dataF = V.head $ select4Vec (pIdx-1) dataF
  where pIdx = truncate $ pVal * (fromIntegral $ V.length dataF)
        select4Vec k = modify (flip H.select (k+1))

theoreticalQuantileV :: Double -> Double -> (Double, Double)
theoreticalQuantileV pVal nu = (hkalCdfStudentRayleighPinv sigma nu pVal, nu)
  where sigma = sqrt $ (nu - 2) / nu


getClosestValueV :: Double -> Vector (Double, Double) -> Double
getClosestValueV basis dat = snd $ V.minimum $ V.zip diff $ V.map snd dat
  where diff = V.map abs $ V.map ((-) basis) $ V.map fst dat

