{-******************************************
  *     File Name: SRMon.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/11/26 13:13:37
  *******************************************-}

import qualified Data.List as L
import qualified Control.Monad as M

-- HasKAL
import qualified HasKAL.MonitorUtils.SRMon.StudentRayleighFunctions as SRF

-- for test code
import qualified HasKAL.MonitorUtils.SRMon.StudentRayleighMon as SRM
import HasKAL.ExternalUtils.GSL.RandomNumberGeneration as RNG
import HasKAL.ExternalUtils.GSL.RandomNumberDistributions as RND

main = do
  rng <- newRngWithSeed (-1)
  dats <- M.forM [1..5] $ \idxI -> M.forM [1..256] $ \idxJ -> do
    r <- gslRanGaussian rng 1.0
    i <- gslRanGaussian rng 1.0
    return $ sqrt (r*r + i*i)

  let nu = srMon 0.99 dats
  print nu
  let nuRe = srMonRe 0.99 dats
  print nuRe



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


srMon :: Double -> [[Double]] -> [Double]
srMon pVal dataFs = Prelude.map (SRM.getOptimalNuQuant pVal) dataFs

