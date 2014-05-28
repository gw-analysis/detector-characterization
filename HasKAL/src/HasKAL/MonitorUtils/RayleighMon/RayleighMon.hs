{-******************************************
  *     File Name: RayleighMon.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/05/28 14:35:56
  *******************************************-}

module HasKAL.MonitorUtils.RayleighMon.RayleighMon(
   rayleighMon
  ,getEmpiricalQuantile
) where

import qualified HasKAL.SpectrumUtils.SpectrumUtils as HSS

{-- Core Functions --}
---- param1: データストライド dT
---- param2: データストライド dF
---- param3: quantile p(x)
---- param3: 時系列データ h(t)
---- return: quantile x(p, f_{j})
rayleighMon :: Int -> Int -> Double -> [Double] -> [Double]
rayleighMon numT numF pVal datT = map (getEmpiricalQuantile pVal) datF'
  where datF' = transposed datF
        datF = map (gwpsd' 1000.0) $ dataSplit numT datT

---- param1: quantile p(x)
---- param2: データセット n(f_j)
---- return: quantile x(p)
getEmpiricalQuantile :: Double -> [Double] -> Double
getEmpiricalQuantile pVal dat = last $ take (truncate (pVal * (realToFrac $ length dat))) $ quicksort dat

{-- Supplementary Functions --}
gwpsd' :: Double -> [Double] -> [Double]
gwpsd' fsample gwDataT = (flip321 HSS.gwpsd fsample $ length gwDataT) gwDataT

-- flip for 3 parameters
flip132 :: (a -> b -> c -> d) -> a -> c -> b -> d
flip132 f = flip.f

flip213 :: (a -> b -> c -> d) -> b -> a -> c -> d
flip213 f = flip f

flip231 :: (a -> b -> c -> d) -> b -> c -> a -> d
flip231 f = flip.(flip f)

flip312 :: (a -> b -> c -> d) -> c -> a -> b -> d
flip312 f = flip (flip.f)

flip321 :: (a -> b -> c -> d) -> c -> b -> a -> d
flip321 f = flip (flip.(flip f))

-- data sort
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
  let smallerOrEqual = [ a | a <- xs, a <= x]
      larger = [a | a <- xs, a > x]
  in quicksort smallerOrEqual ++ [x] ++ quicksort larger

-- transposed 2D list
transposed :: [[Double]] -> [[Double]]
transposed xxs = [ concat $ map ((drop (m-1)).(take m)) xxs | m <- [1..(length (head xxs))] ]
-- [ [h_1(f=0), h_1(f=1), ..], [h_2(f=0), h_2(f=1), ..], ..] -> [ [h_1(f=0), h_2(f=0), ..], [h_1(f=1), h_2(f=1), ..], ..]

-- divide list xs into n point data
dataSplit :: Int -> [Double] -> [[Double]]
dataSplit n xs = [drop (n*(m-1)) (take (m*n) xs) | m <- [1..((length xs) `div` n)]]
-- [h(t)] -> [ [h_1(t)], [h_2(t)], ..]
-- n: Chunk Size

-- n-th data pickup from List xs
pickups :: [Int] -> [Double] -> [Double]
pickups ns xs = concat [((drop (n-1)).(take n)) xs | n <- ns]

