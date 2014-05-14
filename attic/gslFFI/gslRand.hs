{-******************************************
  *     File Name: gslTest.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/04/24 18:48:52
  *******************************************-}

--module GslFunctions(gslGaussRand) where

import qualified Bindings.Gsl.RandomNumberDistributions as BGRND
import qualified Bindings.Gsl.RandomNumberGeneration as BGRNG
import qualified Control.Monad as CM
import qualified Data.List as DL
import qualified Foreign.C.Types as FCT
import qualified Foreign.Ptr as FP
import qualified System.IO as SIO
import qualified System.IO.Unsafe as SIOU
import qualified System.Posix.Time as SPT

import qualified StrictMapping as SM

-- gslGaussRand :: Int -> Double -> [Double]
-- gslGaussRand num sigma = SIOU.unsafePerformIO $ gslGaussRandCore num sigma

gslGaussRandCore :: Int -> Double -> IO () --[Double]
gslGaussRandCore num sigma = do
  let iniSeed = epochT_2_Enum SPT.epochTime :: FCT.CULong                 {- time_t iniSeed = time(NULL);                           -}
  rType <- BGRNG.c'gsl_rng_env_setup :: IO (FP.Ptr BGRNG.C'gsl_rng_type)  {- gsl_rng_type *rType = (gsl_rng_type *)gsl_rng_default; -}
  rSeed <- BGRNG.c'gsl_rng_alloc rType :: IO (FP.Ptr BGRNG.C'gsl_rng)     {- gsl_rng *rSeed = gsl_rng_alloc(rType);                 -}
  BGRNG.c'gsl_rng_set rSeed iniSeed :: IO ()                              {- gsl_rng_set(rSeed, iniSeed);                           -}

  putStrLn "check 1"

  rVal <- SM.forM' [1..num] $ \idx ->                                      {- for(idx=1;idx<=num;idx++){                             -}
    BGRND.c'gsl_ran_gaussian rSeed (realToFrac sigma) :: IO FCT.CDouble   {-   gsl_ran_gaussian(rSeed, sigma); }                    -}

  putStrLn "check 2"

  BGRNG.c'gsl_rng_free rSeed  :: IO ()                                    {- gsl_rng_free(rSeed);                                   -}

  --let x = map realToFrac rVal :: [Double]
  return ()

epochT_2_Enum :: IO FCT.CTime -> FCT.CULong
epochT_2_Enum = toEnum.fromEnum.SIOU.unsafePerformIO

{-- Test Code 
 -- Compile: ghc -o gslTest gslTest.hs --}
main :: IO ()
main = gslGaussRandCore 1000000 1.0


