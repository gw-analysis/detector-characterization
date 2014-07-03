{-******************************************
  *     File Name: StudentRayleighMon.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/07/03 19:43:06
  *******************************************-}

-- Reference
---- [1] C.Rover, Phys. Rev. D 84, 122004 (2011)

module HasKAL.MonitorUtils.SRMon.StudentRayleighMon(
   studentRayleighMon
  ,baseStudentRayleighMon
  ,getOptimalNuLSM -- 後で隠す
--  ,getOptimalNuMLE -- 後で隠す
  ,getOptimalNuQuant -- 後で隠す
  ) where

import qualified Data.List as DL
import qualified Data.Packed.Vector as DPV -- for dataSplit
import qualified Foreign.Storable as FS -- for dataSplit
import qualified HROOT as HROOT -- for histgram
import qualified System.IO.Unsafe as SIOU

import qualified HasKAL.SpectrumUtils.SpectrumUtils as HSS
import qualified HasKAL.Misc.Flip3param as HMF
import qualified HasKAL.MonitorUtils.SRMon.StudentRayleighFunctions as HMSRF

{--  Test Code  --}
-- main :: IO ()
-- main = do
--   print $ studentRayleighMon 100 10 100.0 [1.0,1.0..] $ take 1000 [0.0, 0.1..]

{-- External Functions --}
---- param1: データチャンクT
---- param2: オーバーラップ p (0 < p < 1)
---- param3: データストライド dT
---- param4: データストライド dF
---- param5: サンプリング 1/dt
---- param6: 両側平均スペクトル Sn(f)
---- param7: 時系列データ n(t)
---- retur8: 自由度 nu(f_{j})
studentRayleighMon :: Int -> Double -> Int -> Int -> Double -> [Double] -> [Double] -> [[Double]]
studentRayleighMon num p numT numF fsample snf noft = map (baseStudentRayleighMon numT numF fsample snf) $ dataSplit num m noft
  where m = truncate $ (fromIntegral num) * p

{-- Internal Functions --}
---- param1: データストライド dT
---- param2: データストライド dF
---- param3: サンプリング 1/dt
---- param4: 両側平均スペクトル Sn(f)
---- param5: 時系列データ n(t)
---- return: 自由度 nu(f_{j})
baseStudentRayleighMon :: Int -> Int -> Double -> [Double] -> [Double] -> [Double]
baseStudentRayleighMon numT numF fsample snf noft = map getOptimalNuLSM $ freqClustering numF woff
  where woff = map (map (*(sqrt 2.0)) ) $ map (flip (zipWith (/)) (map sqrt snf)) noff
        noff = map (map sqrt) $ map (map snd) $ map (HMF.flip231 HSS.gwpsd numT fsample) $ dataSplit numT 0 noft

-- nu決定ルーチン(最小二乗法)
---- param1: 規格化されたデータセット w(f_{j=j0})
---- return: 自由度 nu(f_{j=j0})
getOptimalNuLSM :: [Double] -> Double
getOptimalNuLSM wfj = snd $ minimum $ zip ls nu
  where ls = map sum $ map (map (**2)) $ map (zipWith (-) (map snd histj)) theoj
        theoj = mapWith3 HMSRF.hkalRanStudentRayleighPdf sigma nu $ map fst histj
        histj = SIOU.unsafePerformIO $ histogram 300 0 15 wfj
        sigma = map sqrt $ zipWith (/) (map (flip (-) 2.0) nu) nu
        nu = [4.0, 5.0..100.0]

-- nu決定ルーチン(最尤法)
---- param1: 規格化されたデータセット w(f_{j=j0})
---- return: 自由度 nu(f_{j=j0})
-- getOptimalNuMLE :: [Double] -> Double
-- getOptimalNuMLE wfj = snd $ maximum $ zip lt nu
--   where lt = map (DL.foldl1' (*)) $ mapWith3 HMSRF.hkalRanStudentRayleighPdf sigma nu wfj :: [Double]
--         sigma = map sqrt $ zipWith (/) (map (flip (-) 2.0) nu) nu
--         nu = [4.0, 5.0..100.0]

-- nu決定ルーチン(quantileベース[2])
---- param1: quantile
---- param2: 規格化されたデータセット w(f_{j=j0})
---- return: 自由度 nu(f_{j=j0})
getOptimalNuQuant :: Double -> [Double] -> Double
getOptimalNuQuant pVal dat = snd $ minimum $ zip diff nu
  where diff = map abs $ map ((-) empij) theoj
        theoj = zipWith (HMF.flip312 HMSRF.hkalCdfStudentRayleighPinv pVal) sigma nu
        empij = last $ take (truncate (pVal * (realToFrac $ length dat))) $ quicksort dat :: Double
        sigma = map sqrt $ zipWith (/) (map (flip (-) 2.0) nu) nu
        nu = [4.0, 5.0..100.0]

freqClustering :: Int -> [[Double]] -> [[Double]]
freqClustering numF woff = dataSplit (numF*(length woff)) 0 $ concat $ DL.transpose woff

{-- Supplementary Functions --}
---- param1: ビン数
---- param2: 下限値
---- param3: 上限値
---- param4: データセット X_{i}
---- return: (ビンの中央値, エントリー数)
histogram :: Int -> Double -> Double -> [Double] -> IO [(Double, Double)]
histogram nbin min max dat = do
  hist <- HROOT.newTH1F "test1" "test2" nbin min max
  mapM (HROOT.fill1 hist) dat
  entryBins <- mapM (HROOT.getBinContent1 hist) [1..nbin]
  lEdgeBins <- mapM (HROOT.getBinLowEdge hist) [1..nbin]
  widthBin <- HROOT.getBinWidth hist 1
  HROOT.delete hist
  return $ zip (map (+(widthBin/2.0)) lEdgeBins) (map (/(widthBin*(fromIntegral $ length dat))) entryBins)

mapWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [[d]]
mapWith3 f [] _ _ = []
mapWith3 f _ [] _ = []
mapWith3 f (x:xs) (y:ys) zs = map (f x y) zs : mapWith3 f xs ys zs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
  let smallerOrEqual = [ a | a <- xs, a <= x]
      larger = [a | a <- xs, a > x]
  in quicksort smallerOrEqual ++ [x] ++ quicksort larger

dataSplit :: (FS.Storable a) => Int -> Int -> [a] -> [[a]]
dataSplit n m xs = map DPV.toList $ map (HMF.flip231 DPV.subVector n $ DPV.fromList xs) $ [0, (n-m)..(length xs)-n]
