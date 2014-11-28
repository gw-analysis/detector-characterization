{-******************************************
  *     File Name: SRMon.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/11/28 14:29:57
  *******************************************-}

import qualified Data.List as L
import qualified Control.Monad as M
import Data.Vector.Unboxed as V

-- HasKAL
import qualified HasKAL.MonitorUtils.SRMon.StudentRayleighFunctions as SRF

-- for test code
import qualified HasKAL.MonitorUtils.SRMon.StudentRayleighMon as SRM
import HasKAL.ExternalUtils.GSL.RandomNumberGeneration as RNG
import HasKAL.ExternalUtils.GSL.RandomNumberDistributions as RND

main = do
  let dsize = 2048
  let csize = 128
  rng <- newRngWithSeed (-1)
  dats <- M.forM [1..csize] $ \idxI -> M.forM [1..dsize] $ \idxJ -> do
    r <- gslRanGaussian rng 1.0
    i <- gslRanGaussian rng 1.0
    return $ sqrt (r*r + i*i)

  let nu = srMon 0.99 dats
  print nu
  let nuRe = srMonRe 0.99 dats
  print nuRe
  let nuV = srMonV 0.99 $ Prelude.map fromList dats
  print nuV

-- unbox vector
srMonV :: Double -> [Vector Double] -> [Double]
srMonV pVal dataFs = Prelude.map (getNuV pVal) dataFs

getNuV :: Double -> Vector Double -> Double
getNuV pVal dataF = getClosestValueV e t
  where e = empiricalQuantileV pVal dataF
        t = V.map (theoreticalQuantileV pVal) $ fromList [2.0, 2.1..100.0]

empiricalQuantileV :: Double -> Vector Double -> Double
empiricalQuantileV pVal dataF = unsafeIndex (sort4Vec dataF) (pIdx -1)
  where pIdx = truncate $ pVal * (fromIntegral $ V.length dataF)
        sort4Vec = fromList.L.sort.toList -- vectorのソートに変更する

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

-- old function
srMon :: Double -> [[Double]] -> [Double]
srMon pVal dataFs = Prelude.map (SRM.getOptimalNuQuant pVal) dataFs

