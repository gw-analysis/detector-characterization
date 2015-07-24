


module HasKAL.ExternalUtils.GSL.RandomNumberGeneration (
   GSLRng(..)
  ,GSLRngType
  ,newRng
  ,newRngWithSeed
  ,gslRngSet
  ,gslRngUniform
) where

import qualified Foreign as F
import qualified Foreign.C.Types as FCT
import qualified Foreign.Ptr as FP
import qualified System.IO.Unsafe as SIOU
import qualified Data.Time.Clock.POSIX as DTCP

newtype GSLRng = ToRng (F.ForeignPtr ())
newtype GSLRngType = ToRngType (FP.Ptr())

{-- Test code --}
-- main = do
--   rng <- newRng
--   gslRngSet rng 100
--   print =<< gslRngUniform rng
--   rng' <- newRngWithSeed 10
--   print =<< gslRngUniform rng'
--   rng'' <- newRngWithSeed -1
--   print =<< gslRngUniform rng''

{--  External Functions  --}
newRng :: IO GSLRng
newRng = newRngCore gslRngMt19937

-- | when seed is negative value, initial seed become unix time
newRngWithSeed :: Integer -- ^ seed
               -> IO GSLRng
newRngWithSeed  rSeed = do
  rng <- newRng
  case rSeed >= 0 of True -> gslRngSet rng rSeed
                     False -> gslRngSet rng $ round $ SIOU.unsafePerformIO $ DTCP.getPOSIXTime
  return rng

gslRngSet :: GSLRng -> Integer -> IO ()
gslRngSet (ToRng cPtr) rSeed = F.withForeignPtr cPtr $ flip gsl_rng_set $ fromInteger rSeed

gslRngUniform :: GSLRng -> IO Double
gslRngUniform (ToRng cPtr) = return.realToFrac =<< F.withForeignPtr cPtr gsl_rng_uniform

{--  Internal Functions  --}
newRngCore :: GSLRngType -> IO GSLRng
newRngCore (ToRngType tptr) = do
  rptr <- gsl_rng_alloc tptr
  cPtr <- F.newForeignPtr gsl_rng_free rptr
  return $ ToRng cPtr

gslRngMt19937 :: GSLRngType
gslRngMt19937 = (ToRngType $ SIOU.unsafePerformIO $ F.peek gsl_rng_mt19937)


{--  C functions  --}
{- p.284 18.3 -- Random number generator initialization -}
foreign import ccall "gsl_rng_alloc" gsl_rng_alloc :: FP.Ptr () -> IO (FP.Ptr ())
foreign import ccall "&gsl_rng_free" gsl_rng_free :: FP.FunPtr (FP.Ptr () -> IO ())
foreign import ccall "gsl_rng_set" gsl_rng_set :: FP.Ptr () -> FCT.CULong -> IO ()

{- p.285 18.4 -- Sampling from a random number generator -}
foreign import ccall "gsl_rng_uniform" gsl_rng_uniform :: FP.Ptr () -> IO FCT.CDouble

{- p.290 18.9 -- Random number generator algorithms -}
foreign import ccall "&gsl_rng_mt19937" gsl_rng_mt19937 :: FP.Ptr (FP.Ptr ())
