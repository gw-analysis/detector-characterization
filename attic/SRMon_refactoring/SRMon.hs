{-******************************************
  *     File Name: SRMon.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/12/04 13:12:11
  *******************************************-}

module SRMon (
   timeShift
  ,srMonM
) where

import Data.List (sort)
import System.IO.Unsafe (unsafePerformIO)
import Data.Vector.Unboxed as V
import Data.Matrix.Unboxed as M
import qualified Control.Monad as CM

-- HasKAL
import qualified HasKAL.MonitorUtils.SRMon.StudentRayleighFunctions as SRF

-- for refactoring code
import qualified Data.List as L (sort, last, take , length, minimum, zip)

timeShift :: (Matrix Double -> Vector Double) -> Int -> Int -> Int -> Vector Double -> Matrix Double -> Matrix Double
timeShift fx chunck shift clusteringF snf mat = do
  fromColumns $ unsafePerformIO $ CM.forM [0, shift..cols wMat - chunck] $ \idx -> do
    return $ fx $ frequencyClusteringM clusteringF $ subMatrix (0, rows wMat - 1) (idx, idx+chunck-1) wMat
  where wMat = whiteningSpectrogram snf mat

-- unbox matrix
srMonM :: Double -> Matrix Double -> Vector Double
srMonM pVal dataFs = mapRows0 (getNuV pVal) $ dataFs

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

-- matrix supply
mapRows0 :: (Unbox a) => (Vector a -> a) -> Matrix a -> Vector a
mapRows0 fx mat = unsafePerformIO $ forM idxV $ \idx -> return $ fx $ takeRow idx mat
  where idxV = fromList [0..(rows mat)-1]

mapColumns0 :: (Unbox a) => (Vector a -> a) -> Matrix a -> Vector a
mapColumns0 fx mat = unsafePerformIO $ forM idxV $ \idx -> return $ fx $ takeColumn idx mat
  where idxV = fromList [0..(cols mat)-1]

mapRows1 :: (Unbox a) => (Vector a -> Vector a -> Vector a) -> Vector a -> Matrix a -> Matrix a
mapRows1 fx vec mat = fromRows $ unsafePerformIO $ CM.forM idxL $ \idx -> return $ fx vec $ takeRow idx mat
  where idxL = [0..(rows mat)-1]

mapColumns1 :: (Unbox a) => (Vector a -> Vector a -> Vector a) -> Vector a -> Matrix a -> Matrix a
mapColumns1 fx vec mat = fromColumns $ unsafePerformIO $ CM.forM idxL $ \idx -> return $ fx vec $ takeColumn idx mat
  where idxL = [0..(cols mat)-1]



-- unbox vector
srMonV :: Double -> [Vector Double] -> [Double]
srMonV pVal dataFs = Prelude.map (getNuV pVal) dataFs

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


-- refactoring functions
srMonRe :: Double -> [[Double]] -> [Double]
srMonRe pVal dataFs = Prelude.map (getNu pVal) dataFs

getNu :: Double -> [Double] -> Double
getNu pVal dataF = getClosestValue e t
  where e = empiricalQuantile pVal dataF
        t = Prelude.map (theoreticalQuantile pVal) [2.0, 2.1..100.0]

empiricalQuantile :: Double -> [Double] -> Double
empiricalQuantile pVal dataF = L.last $ L.take pIdx $ L.sort dataF
  where pIdx = truncate $ pVal * (fromIntegral $ L.length dataF)

theoreticalQuantile :: Double -> Double -> (Double, Double)
theoreticalQuantile pVal nu = (SRF.hkalCdfStudentRayleighPinv sigma nu pVal, nu)
  where sigma = sqrt $ (nu - 2) /nu

getClosestValue :: Double -> [(Double, Double)] -> Double
getClosestValue basis dat = snd $ L.minimum $ L.zip diff $ Prelude.map snd dat
  where diff = Prelude.map abs $ Prelude.map ((-) basis) $ Prelude.map fst dat


