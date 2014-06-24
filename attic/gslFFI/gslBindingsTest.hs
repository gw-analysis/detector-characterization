{-******************************************
  *     File Name: gslBindingsTest.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/06/24 12:04:09
  *******************************************-}

import qualified Bindings.Gsl.RandomNumberDistributions as RND
import qualified Bindings.Gsl.RandomNumberGeneration as RNG
import qualified Foreign.C.Types as FCT
import qualified Foreign.Ptr as FP
import qualified System.IO.Unsafe as SIOU
import qualified System.Posix.Time as SPT
import qualified HasKAL.Misc.StrictMapping as SM

{-- Test Code 
 -- Compile: ghc -o gslTest gslTest.hs --}
main :: IO ()
main = do 
  let iniSeed = epochT_2_Enum SPT.epochTime :: FCT.CULong             {- time_t iniSeed = time(NULL);                           -}
  rType <- RNG.c'gsl_rng_env_setup :: IO (FP.Ptr RNG.C'gsl_rng_type)  {- gsl_rng_type *rType = (gsl_rng_type *)gsl_rng_default; -}
  rSeed <- RNG.c'gsl_rng_alloc rType :: IO (FP.Ptr RNG.C'gsl_rng)     {- gsl_rng *rSeed = gsl_rng_alloc(rType);                 -}
  RNG.c'gsl_rng_set rSeed iniSeed :: IO ()                            {- gsl_rng_set(rSeed, iniSeed);                           -}
                                                                                                                                  
  rVal <- SM.forM' [1..10] $ \idx ->                                  {- for(idx=1;idx<=10;idx++){                              -}
    RND.c'gsl_ran_gaussian rSeed (realToFrac 1.0) :: IO FCT.CDouble   {-   rVal[idx] = gsl_ran_gaussian(rSeed, sigma); }        -}
                                                                                                                                  
  print rVal                                                          {- for(idx=i;idx<=10;idx++) printf("%e\n", rVal);         -}
  RNG.c'gsl_rng_free rSeed  :: IO ()                                  {- gsl_rng_free(rSeed);                                   -}

epochT_2_Enum :: IO FCT.CTime -> FCT.CULong
epochT_2_Enum = toEnum.fromEnum.SIOU.unsafePerformIO

