{-******************************************
  *     File Name: RayleighMon.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/04/10 21:16:51
  *******************************************-}

module HasKAL.MonitorUtils.RayleighMon(rayleighMon) where

import qualified HasKAL.SpectrumUtils.SpectrumUtils as HSS

-- core function
rayleighMon :: Int -> Int -> Double -> [Double] -> [[Double]]
rayleighMon dTNum strideNum fsample gwDataT = do

  -- threshold
  let pVal = [0.5, 0.7, 0.8, 0.9] :: [Double]
  let pNum = map floor $ map ((fromIntegral ((length gwDataT) `div` dTNum)) *) pVal :: [Int]

  -- format: [ [h_1(t)], [h_2(t)], ..]
  let gwDataT' = dataSplit dTNum gwDataT :: [[Double]]

  -- format: [ [<h_1(f=0)>, <h_2(f=0)>, ..], [<h_1(f=1)>, <h_2(f=1)>, ..], ..]
  let gwDataF' = take (strideNum `div` 2 + 1) $ transposed $ map (flip231 HSS.gwpsd strideNum fsample) gwDataT' :: [[Double]]
  --                            ^
  --                   片側スペクトルのみ取り出す


  -- sort each frequency bin
  let gwSortDataF' = map quicksort gwDataF'

  map (pickups pNum) gwSortDataF'  


-- [h(t)] -> [ [h_1(t)], [h_2(t)], ..]
-- n: Chunk Size
dataSplit :: Int -> [Double] -> [[Double]]
dataSplit n xs = [drop (n*(m-1)) (take (m*n) xs) | m <- [1..((length xs) `div` n)]]


-- [ [h_1(f=0), h_1(f=1), ..], [h_2(f=0), h_2(f=1), ..], ..] -> [ [h_1(f=0), h_2(f=0), ..], [h_1(f=1), h_2(f=1), ..], ..]
transposed :: [[Double]] -> [[Double]]
transposed xxs = [ concat $ map ((drop (m-1)).(take m)) xxs | m <- [1..(length (head xxs))] ]


-- data sort
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
  let smallerOrEqual = [ a | a <- xs, a <= x]
      larger = [a | a <- xs, a > x]
  in quicksort smallerOrEqual ++ [x] ++ quicksort larger


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

pickups :: [Int] -> [Double] -> [Double]
pickups ns xs = concat [((drop (n-1)).(take n)) xs | n <- ns]

