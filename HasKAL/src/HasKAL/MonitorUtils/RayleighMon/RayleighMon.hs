{-******************************************
  *     File Name: RayleighMon.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/06/23 15:07:18
  *******************************************-}

module HasKAL.MonitorUtils.RayleighMon.RayleighMon(
   rayleighMon
  ,rayleighMon'
  ,getEmpiricalQuantile
) where

import qualified Foreign.Storable as FS
import qualified Data.Packed.Vector as DPV
import qualified Data.List as DL

import qualified HasKAL.Misc.Flip3param as HMF
import qualified HasKAL.SpectrumUtils.SpectrumUtils as HSS

{-- Core Functions --}
---- param1: データストライド dT
---- param2: データストライド dF
---- param3: サンプリング fs
---- param4: quantile p(x)
---- param5: 時系列データ n(t)
---- return: quantile x(p=p0, f_{j})
rayleighMon :: Int -> Int -> Double -> Double -> [Double] -> [Double]
rayleighMon numT numF fsample pVal datT = map (getEmpiricalQuantile pVal) $ (dataSplit (numF*(length datF)) 0).concat.DL.transpose $ datF
  where datF = map (map snd) $ map (HMF.flip231 HSS.gwpsd numT fsample) $ dataSplit numT 0 datT
---- param1: データストライド dT
---- param2: データストライド dF
---- param3: サンプリング fs
---- param4: quantile p(x)
---- param5: 両側平均スペクトル Sn(f)
---- param6: 時系列データ n(t)
---- return: quantile x(p=p0, f_{j})
rayleighMon' :: Int -> Int -> Double -> Double -> [Double] -> [Double] -> [Double]
rayleighMon' numT numF fsample pVal snf datT = map (getEmpiricalQuantile pVal) $ (dataSplit (numF*(length datFW)) 0).concat.DL.transpose $ datFW
  where datFW = map (map (*(sqrt 2.0)) ) $ map (flip (zipWith (/)) (map sqrt snf)) datF
        datF = map (map sqrt) $ map (map snd) $ map (HMF.flip231 HSS.gwpsd numT fsample) $ dataSplit numT 0 datT

---- param1: quantile
---- param2: データセット n(f_j)
---- return: quantile x(p)
getEmpiricalQuantile :: Double -> [Double] -> Double
getEmpiricalQuantile pVal dat = last $ take (truncate (pVal * (realToFrac $ length dat))) $ quicksort dat

{-- Supplementary Functions --}
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
  let smallerOrEqual = [ a | a <- xs, a <= x]
      larger = [a | a <- xs, a > x]
  in quicksort smallerOrEqual ++ [x] ++ quicksort larger

dataSplit :: (FS.Storable a) => Int -> Int -> [a] -> [[a]]
dataSplit n m xs = map DPV.toList $ map (HMF.flip231 DPV.subVector n $ DPV.fromList xs) $ [0, (n-m)..(length xs)-n]

