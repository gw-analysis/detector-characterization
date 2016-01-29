


module HasKAL.MonitorUtils.RayleighMon.RayleighMon (
   rayleighMon
  ,rayleighMonV
  ,rayleighMonWaveData
  ,rayleighHist
) where

import Prelude as P
import Data.List (transpose)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Packed.Vector as PV (subVector, fromList, toList, dim)
import qualified Data.Packed.Matrix as PM (cols, rows, toRows, fromColumns)
import Data.Vector.Unboxed as V
import Data.Matrix.Unboxed as M hiding ((!), convert)
import qualified Control.Monad as CM (forM)
import qualified Data.Vector.Algorithms.Heap as H (sort, select)

import HasKAL.Misc.UMatrixMapping
import HasKAL.SpectrumUtils.Signature (Spectrum, Spectrogram)
import HasKAL.SpectrumUtils.Function
import HasKAL.SpectrumUtils.SpectrumUtils (gwOnesidedPSDV, gwspectrogramV, gwOnesidedPSDWaveData, gwspectrogramWaveData)
import HasKAL.ExternalUtils.GSL.RandomNumberDistributions (gslCdfRayleighPinv)
import HasKAL.WaveUtils.Data (WaveData(..))

{-- Expose Functions --}
-- | rayleigh monitor
rayleighMon :: [Double] -> Double -> Double -> Double -> [Double] -> [Double] -> [([(Double, Double)], [(Double, Double)])]
rayleighMon pVals fs fftSec df snt ht = fromSpectrumPair $ rayleighMonV pVals fs nfft ndf snf hfs
    where nfft = truncate (fftSec * fs)
          ndf = truncate (df * fftSec)
          snf = gwOnesidedPSDV (PV.fromList snt) nfft fs
          hfs = gwspectrogramV 0 nfft fs $ PV.fromList ht
          fromSpectrumPair xs = unsafePerformIO $ CM.forM xs $ \(x,y) -> return $ (fromSpectrum x, fromSpectrum y)

rayleighMonWaveData :: [Double] -> Double -> Double -> WaveData -> WaveData -> [(Spectrum, Spectrum)]
rayleighMonWaveData pVals fftSec df snt ht = rayleighMonV pVals fs nfft ndf snf hfs
  where fs = samplingFrequency snt
        nfft = truncate (fftSec * fs)
        ndf = truncate (df * fftSec)
        snf = gwOnesidedPSDWaveData fftSec snt
        hfs = gwspectrogramWaveData 0 fftSec ht

-- | rayleigh monitor (vector I/O)
rayleighMonV :: [Double] -- ^ pValues
             -> Double -- ^ sampling rate [Hz]
             -> Int -- ^ stride
             -> Int -- ^ df
             -> Spectrum -- ^ averaged spectrum Sn(f)
             -> Spectrogram -- ^ spectrogram h(t, f)
             -> [(Spectrum, Spectrum)]
rayleighMonV pVals fsample stride fClust (freqV1, specV1) (tV2, freqV2, specM2) = do
  let snf' = V.map (sqrt.(*0.5)) $ convert $ specV1
      hfs' = M.map sqrt $ convertS2U $ specM2
      wMat = whiteningSpectrogram snf' hfs'
      df = fromIntegral fClust * fsample / fromIntegral stride
      newFV = fromList [df, df*2 .. df * fromIntegral ((V.length $ convert freqV2)`div`fClust)]
  unsafePerformIO $ CM.forM pVals $ \pVal -> do
        let noiseLv = rMonM pVal $ frequencyClusteringM fClust wMat
            theorem = gslCdfRayleighPinv pVal 1.0
            theoremV = (convert $ fromList [0.0, V.last newFV], convert $ fromList [theorem, theorem])
        return $ ((convert newFV, convert noiseLv), theoremV)

rayleighHist :: [[Spectrum]] -> [Spectrogram]
rayleighHist xss = P.zip3 (repeat freqV) (repeat (PV.fromList $ P.init bins)) yields
  where freqV = (fst . P.head . P.head) xss
        bins = [0,0.1..10]
        yields = P.map (PM.fromColumns . P.map (PV.fromList . snd . histogram1d 0 0 bins . PV.toList)
                      . PM.toRows . PM.fromColumns . (P.map snd)) . transpose $ xss
        histogram1d xmin xmax bins input =
          let intervals = P.zipWith (\x y ->(x, y)) (P.init bins) (P.tail bins)
              within u x = x >= fst u && x < snd u
           in (P.map fst intervals, P.map ((fromIntegral.P.length) . (\u -> P.filter (within u) input)) intervals)


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
frequencyClusteringM num mat = fromVector (newRow, newCol) $ slice 0 (newCol*newRow) $ flatten $ mat
  where newCol = num * oldCol
        newRow = oldRow*oldCol `div` newCol
        oldCol = cols mat
        oldRow = rows mat

whiteningSpectrogram :: Vector Double -> Matrix Double -> Matrix Double
whiteningSpectrogram snf hfs = mapCols1 whiteningSpectrum snf hfs

whiteningSpectrum :: Vector Double -> Vector Double -> Vector Double
whiteningSpectrum snf hf = V.zipWith (/) hf snf

getEmpiricalQuantile :: Double -> Vector Double -> Double
-- getEmpiricalQuantile pVal datV = V.head $ V.drop (pIdx-1) $ sort4Vec datV
getEmpiricalQuantile pVal datV = V.head $ select4Vec (pIdx-1) datV
  where pIdx = truncate $ pVal * (fromIntegral $ V.length datV)
        -- sort4Vec = modify H.sort
        select4Vec k = modify (flip H.select (k+1))
