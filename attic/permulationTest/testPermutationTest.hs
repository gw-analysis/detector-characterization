{-
- to compile this
-ghc --make testPermutationTest.hs HasKAL/StatisticsUtils/Functions.hs HasKAL/StatisticsUtils/statisticsUtils.c -L/play/kagra/soft/gsl/lib -lgsl -lgslcblas
-
- on ghci
- firtst off,  you may want to generate library like following
- gcc -c statisticsUtils.c
- ar r libstatisticsUtils.a statisticsUtils.o
- then,
- ghci HasKAL/StatisticsUtils/Functions.hs -LHasKAL/StatisticsUtils
- -lstatisticsUtils -L/play/kagra/soft/gsl/lib -lgsl -lgslcblas

- Yuzurihara wrote and added these compile line in "makefile".
-}

import HasKAL.StatisticsUtils.Functions
--import System.Random
--import System.IO

main :: IO ()
main = do
--  let x = take 1000 $ randomRs (-1, 1) $ mkStdGen 1 :: [Double]
--      y = take 1000 $ randomRs (-1, 1) $ mkStdGen 2 :: [Double]

  channel1 <- readFile "data1.txt"
  channel2 <- readFile "data2.txt"
  let x = map read $ lines channel1 :: [Double]
  let y = map read $ lines channel2 :: [Double]
--  let x = [1,2,3]
--  let y = [1,2,4]
  --print y
  print $ calculatePeasonCorrelation x y
  print "hoge"
  print $ permutationTestPeasonCorrelation x y 100
