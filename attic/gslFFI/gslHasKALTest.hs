{-******************************************
  *     File Name: gslHasKALTest.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/06/24 12:03:34
  *******************************************-}

import qualified HasKAL.ExternalUtils.GSL.RandomNumberGeneration as RNG
import qualified HasKAL.ExternalUtils.GSL.RandomNumberDistributions as RND
import qualified HasKAL.Misc.StrictMapping as SM

{-- Test Code 
 -- Compile: ghc -o gslTest gslTest.hs --}
main :: IO ()
main = do 
  rSeed <- RNG.newRngWithSeed (-1) :: IO RNG.GSLRng   {- time_t iniSeed = time(NULL);                           -}
                                                      {- gsl_rng_type *rType = (gsl_rng_type *)gsl_rng_default; -}
                                                      {- gsl_rng *rSeed = gsl_rng_alloc(rType);                 -}
                                                      {- gsl_rng_set(rSeed, iniSeed);                           -}
                                                                                                                  
  rVal <- SM.forM' [1..10] $ \idx ->                  {- for(idx=1;idx<=10;idx++){                              -}
    RND.gslRanGaussian rSeed 1.0  :: IO Double        {-   rVal[idx] = gsl_ran_gaussian(rSeed, sigma); }        -}
                                                                                                                  
  print rVal :: IO ()                                 {- for(idx=i;idx<=10;idx++) printf("%e\n", rVal);         -}

