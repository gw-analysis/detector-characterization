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
-}

import HasKAL.StatisticsUtils.Functions
import System.Random

main :: IO ()
main = do
  let x = take 1000 $ randomRs (-1, 1) $ mkStdGen 1 :: [Double]
      y = take 1000 $ randomRs (-1, 1) $ mkStdGen 2 :: [Double]

  print $ permutationTestPeasonCorrelation x y 10


