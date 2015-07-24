


module HasKAL.ExternalUtils.GSL.RandomNumberDistributions (
  ---- Gaussian Distribution
   gslRanGaussian
  ,gslRanGaussianPdf
  ,gslCdfGaussianP
  ,gslCdfGaussianQ
  ,gslCdfGaussianPinv
  ,gslCdfGaussianQinv
  ---- Exponential Distribution
  ,gslRanExponential
  ,gslRanExponentialPdf
  ,gslCdfExponentialP
  ,gslCdfExponentialQ
  ,gslCdfExponentialPinv
  ,gslCdfExponentialQinv
  ---- Flat Distribution
  ,gslRanFlat
  ,gslRanFlatPdf
  ,gslCdfFlatP
  ,gslCdfFlatQ
  ,gslCdfFlatPinv
  ,gslCdfFlatQinv
  ---- Rayleigh Distribution
  ,gslRanRayleigh
  ,gslRanRayleighPdf
  ,gslCdfRayleighP
  ,gslCdfRayleighQ
  ,gslCdfRayleighPinv
  ,gslCdfRayleighQinv
  ---- Chi-Squared Distribution
  ,gslRanChisq
  ,gslRanChisqPdf
  ,gslCdfChisqP
  ,gslCdfChisqQ
  ,gslCdfChisqPinv
  ,gslCdfChisqQinv
  ---- F Distribution
  ,gslRanFdist
  ,gslRanFdistPdf
  ,gslCdfFdistP
  ,gslCdfFdistQ
  ,gslCdfFdistPinv
  ,gslCdfFdistQinv
  ---- Student-t Distribution
  ,gslRanTdist
  ,gslRanTdistPdf
  ,gslCdfTdistP
  ,gslCdfTdistQ
  ,gslCdfTdistPinv
  ,gslCdfTdistQinv
  ---- Poisson Distribution
  ,gslRanPoisson
  ,gslRanPoissonPdf
  ,gslCdfPoissonP
  ,gslCdfPoissonQ
) where

import qualified Foreign as F -- for random generator
import qualified Foreign.Ptr as FP -- for random generator
import qualified Foreign.C.Types as FCT -- for distribution functions

import qualified HasKAL.ExternalUtils.GSL.RandomNumberGeneration as RNG
import qualified HasKAL.Misc.Flip3param as HMF

{- p.309 20.2 -- Gaussian distribution -}
foreign import ccall "gsl_ran_gaussian" gsl_ran_gaussian :: FP.Ptr () -> FCT.CDouble -> IO FCT.CDouble
gslRanGaussian :: RNG.GSLRng
               -> Double -- ^ sigma
               -> IO Double
gslRanGaussian (RNG.ToRng cPtr) sigma = return.realToFrac =<< F.withForeignPtr cPtr (flip gsl_ran_gaussian $ realToFrac sigma)


foreign import ccall "gsl_ran_gaussian_pdf" gsl_ran_gaussian_pdf :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble
gslRanGaussianPdf :: Double -- ^ x
                  -> Double -- ^ sigma
                  -> Double -- ^ p(x)
gslRanGaussianPdf x sigma = realToFrac $ gsl_ran_gaussian_pdf (realToFrac x) (realToFrac sigma)


foreign import ccall "gsl_cdf_gaussian_P" gsl_cdf_gaussian_P :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble
gslCdfGaussianP :: Double -- ^ x
                -> Double -- ^ sigma 
                -> Double -- ^ P(x)
gslCdfGaussianP x sigma = realToFrac $ gsl_cdf_gaussian_P (realToFrac x) (realToFrac sigma)

foreign import ccall "gsl_cdf_gaussian_Q" gsl_cdf_gaussian_Q :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble
gslCdfGaussianQ :: Double -- ^ x
                -> Double -- ^ sigma
                -> Double -- ^ Q(x)
gslCdfGaussianQ x sigma = realToFrac $ gsl_cdf_gaussian_Q (realToFrac x) (realToFrac sigma)

foreign import ccall "gsl_cdf_gaussian_Pinv" gsl_cdf_gaussian_Pinv :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble
gslCdfGaussianPinv :: Double -- ^ P(x)
                   -> Double -- ^ sigma
                   -> Double -- ^ x
gslCdfGaussianPinv pVal sigma = realToFrac $ gsl_cdf_gaussian_Pinv (realToFrac pVal) (realToFrac sigma)

foreign import ccall "gsl_cdf_gaussian_Qinv" gsl_cdf_gaussian_Qinv :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble
gslCdfGaussianQinv :: Double -- ^ Q(x)
                   -> Double -- ^ sigma
                   -> Double -- ^ x
gslCdfGaussianQinv qVal sigma = realToFrac $ gsl_cdf_gaussian_Qinv (realToFrac qVal) (realToFrac sigma)



{- p.316 20.5 -- Exponential distribution -}
foreign import ccall "gsl_ran_exponential" gsl_ran_exponential :: FP.Ptr () -> FCT.CDouble -> IO FCT.CDouble
gslRanExponential :: RNG.GSLRng 
                  -> Double -- ^ mean
                  -> IO Double
gslRanExponential (RNG.ToRng cPtr) mu = return.realToFrac =<< F.withForeignPtr cPtr (flip gsl_ran_exponential $ realToFrac mu)

foreign import ccall "gsl_ran_exponential_pdf" gsl_ran_exponential_pdf :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble
gslRanExponentialPdf :: Double -- ^ x
                     -> Double -- ^ mean
                     -> Double -- ^ p(x)
gslRanExponentialPdf x mu = realToFrac $ gsl_ran_exponential_pdf (realToFrac x) (realToFrac mu)

foreign import ccall "gsl_cdf_exponential_P" gsl_cdf_exponential_P :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble
gslCdfExponentialP :: Double -- ^ x
                   -> Double -- ^ mean
                   -> Double -- ^ P(x)
gslCdfExponentialP x mu = realToFrac $ gsl_cdf_exponential_P (realToFrac x) (realToFrac mu)

foreign import ccall "gsl_cdf_exponential_Q" gsl_cdf_exponential_Q :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble
gslCdfExponentialQ :: Double -- ^ x
                   -> Double -- ^ mean
                   -> Double -- ^ Q(x)
gslCdfExponentialQ x mu = realToFrac $ gsl_cdf_exponential_Q (realToFrac x) (realToFrac mu)

foreign import ccall "gsl_cdf_exponential_Pinv" gsl_cdf_exponential_Pinv :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble
gslCdfExponentialPinv :: Double -- ^ P(x)
                      -> Double -- ^ mean
                      -> Double -- ^ x
gslCdfExponentialPinv pVal mu = realToFrac $ gsl_cdf_exponential_Pinv (realToFrac pVal) (realToFrac mu)

foreign import ccall "gsl_cdf_exponential_Qinv" gsl_cdf_exponential_Qinv :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble
gslCdfExponentialQinv :: Double -- ^ Q(x)
                      -> Double -- ^ mean
                      -> Double -- ^ x
gslCdfExponentialQinv qVal mu = realToFrac $ gsl_cdf_exponential_Qinv (realToFrac qVal) (realToFrac mu)



{- p.324 20.9 -- Rayleigh distribution -}
foreign import ccall "gsl_ran_rayleigh" gsl_ran_rayleigh :: FP.Ptr () -> FCT.CDouble -> IO FCT.CDouble
gslRanRayleigh :: RNG.GSLRng 
               -> Double -- ^ sigma
               -> IO Double
gslRanRayleigh (RNG.ToRng cPtr) sigma = return.realToFrac =<< F.withForeignPtr cPtr (flip gsl_ran_rayleigh $ realToFrac sigma)

foreign import ccall "gsl_ran_rayleigh_pdf" gsl_ran_rayleigh_pdf :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble
gslRanRayleighPdf :: Double -- ^ x
                  -> Double -- ^ sigma
                  -> Double -- ^ p(x)
gslRanRayleighPdf x sigma = realToFrac $ gsl_ran_rayleigh_pdf (realToFrac x) (realToFrac sigma)

foreign import ccall "gsl_cdf_rayleigh_P" gsl_cdf_rayleigh_P :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble
gslCdfRayleighP :: Double -- ^ x
                -> Double -- ^ sigma
                -> Double -- ^ P(x)
gslCdfRayleighP x sigma = realToFrac $ gsl_cdf_rayleigh_P (realToFrac x) (realToFrac sigma)

foreign import ccall "gsl_cdf_rayleigh_Q" gsl_cdf_rayleigh_Q :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble
gslCdfRayleighQ :: Double -- ^ x
                -> Double -- ^ sigma
                -> Double -- ^ Q(x)
gslCdfRayleighQ x sigma = realToFrac $ gsl_cdf_rayleigh_Q (realToFrac x) (realToFrac sigma)

foreign import ccall "gsl_cdf_rayleigh_Pinv" gsl_cdf_rayleigh_Pinv :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble
gslCdfRayleighPinv :: Double -- ^ P(x)
                   -> Double -- ^ sigma
                   -> Double -- ^ x
gslCdfRayleighPinv pVal sigma = realToFrac $ gsl_cdf_rayleigh_Pinv (realToFrac pVal) (realToFrac sigma)

foreign import ccall "gsl_cdf_rayleigh_Qinv" gsl_cdf_rayleigh_Qinv :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble
gslCdfRayleighQinv :: Double -- ^ Q(x)
                   -> Double -- ^ sigma
                   -> Double -- ^ x
gslCdfRayleighQinv qVal sigma = realToFrac $ gsl_cdf_rayleigh_Qinv (realToFrac qVal) (realToFrac sigma)



{- p332 20.15 -- Flat distribution -}
foreign import ccall "gsl_ran_flat" gsl_ran_flat :: FP.Ptr () -> FCT.CDouble -> FCT.CDouble -> IO FCT.CDouble
gslRanFlat :: RNG.GSLRng 
           -> Double -- ^ minimum a
           -> Double -- ^ maximum b
           -> IO Double
gslRanFlat (RNG.ToRng cPtr) a b = return.realToFrac =<< F.withForeignPtr cPtr (HMF.flip231 gsl_ran_flat (realToFrac a) (realToFrac b))

foreign import ccall "gsl_ran_flat_pdf" gsl_ran_flat_pdf :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble -> FCT.CDouble
gslRanFlatPdf :: Double -- ^ x
              -> Double -- ^ a
              -> Double -- ^ b
              -> Double -- ^ p(x)
gslRanFlatPdf x a b  = realToFrac $ gsl_ran_flat_pdf (realToFrac x) (realToFrac a) (realToFrac b)

foreign import ccall "gsl_cdf_flat_P" gsl_cdf_flat_P :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble -> FCT.CDouble
gslCdfFlatP :: Double -- ^ x
            -> Double -- ^ a
            -> Double -- ^ b
            -> Double -- ^ P(x)
gslCdfFlatP x a b = realToFrac $ gsl_cdf_flat_P (realToFrac x) (realToFrac a) (realToFrac b)

foreign import ccall "gsl_cdf_flat_Q" gsl_cdf_flat_Q :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble -> FCT.CDouble
gslCdfFlatQ :: Double -- ^ x
            -> Double -- ^ a
            -> Double -- ^ b
            -> Double -- ^ Q(x)
gslCdfFlatQ x a b = realToFrac $ gsl_cdf_flat_Q (realToFrac x) (realToFrac a) (realToFrac b)

foreign import ccall "gsl_cdf_flat_Pinv" gsl_cdf_flat_Pinv :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble -> FCT.CDouble
gslCdfFlatPinv :: Double -- ^ P(x)
               -> Double -- ^ a
               -> Double -- ^ b
               -> Double -- ^ x
gslCdfFlatPinv pVal a b = realToFrac $ gsl_cdf_flat_Pinv (realToFrac pVal) (realToFrac a) (realToFrac b)

foreign import ccall "gsl_cdf_flat_Qinv" gsl_cdf_flat_Qinv :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble -> FCT.CDouble
gslCdfFlatQinv :: Double -- ^ Q(x)
               -> Double -- ^ a
               -> Double -- ^ b
               -> Double -- ^ x
gslCdfFlatQinv qVal a b = realToFrac $ gsl_cdf_flat_Qinv (realToFrac qVal) (realToFrac a) (realToFrac b)



{- p336 20.17 -- Chi-Squared distribution -}
foreign import ccall "gsl_ran_chisq" gsl_ran_chisq :: FP.Ptr () -> FCT.CDouble -> IO FCT.CDouble
gslRanChisq :: RNG.GSLRng 
            -> Double -- ^ d.o.f nu
            -> IO Double
gslRanChisq (RNG.ToRng cPtr) dof = return.realToFrac =<< F.withForeignPtr cPtr (flip gsl_ran_chisq $ realToFrac dof)

foreign import ccall "gsl_ran_chisq_pdf" gsl_ran_chisq_pdf :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble
gslRanChisqPdf :: Double -- ^ x
               -> Double -- ^ nu
               -> Double -- ^ p(x)
gslRanChisqPdf x dof = realToFrac $ gsl_ran_chisq_pdf (realToFrac x) (realToFrac dof)

foreign import ccall "gsl_cdf_chisq_P" gsl_cdf_chisq_P :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble
gslCdfChisqP :: Double -- ^ x
             -> Double -- ^ nu
             -> Double -- ^ P(x)
gslCdfChisqP x dof = realToFrac $ gsl_cdf_chisq_P (realToFrac x) (realToFrac dof)

foreign import ccall "gsl_cdf_chisq_Q" gsl_cdf_chisq_Q :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble
gslCdfChisqQ :: Double -- ^ x
             -> Double -- ^ nu
             -> Double -- ^ Q(x)
gslCdfChisqQ x dof = realToFrac $ gsl_cdf_chisq_Q (realToFrac x) (realToFrac dof)

foreign import ccall "gsl_cdf_chisq_Pinv" gsl_cdf_chisq_Pinv :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble
gslCdfChisqPinv :: Double -- ^ P(x)
                -> Double -- ^ nu
                -> Double -- ^ x
gslCdfChisqPinv pVal dof = realToFrac $ gsl_cdf_chisq_Pinv (realToFrac pVal) (realToFrac dof)

foreign import ccall "gsl_cdf_chisq_Qinv" gsl_cdf_chisq_Qinv :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble
gslCdfChisqQinv :: Double -- ^ Q(x)
                -> Double -- ^ nu
                -> Double -- ^ x
gslCdfChisqQinv qVal dof = realToFrac $ gsl_cdf_chisq_Qinv (realToFrac qVal) (realToFrac dof)



{- p338 20.18 -- F distribution -}
foreign import ccall "gsl_ran_fdist" gsl_ran_fdist :: FP.Ptr () -> FCT.CDouble -> FCT.CDouble -> IO FCT.CDouble
gslRanFdist :: RNG.GSLRng 
            -> Double -- ^ d.o.f nu1
            -> Double -- ^ d.o.f nu2
            -> IO Double
gslRanFdist (RNG.ToRng cPtr) dof1 dof2 = return.realToFrac =<< F.withForeignPtr cPtr (HMF.flip231 gsl_ran_fdist (realToFrac dof1) (realToFrac dof2))

foreign import ccall "gsl_ran_fdist_pdf" gsl_ran_fdist_pdf :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble -> FCT.CDouble
gslRanFdistPdf :: Double -- ^ x
               -> Double -- ^ nu1
               -> Double -- ^ nu2
               -> Double -- ^ p(x)
gslRanFdistPdf x dof1 dof2  = realToFrac $ gsl_ran_fdist_pdf (realToFrac x) (realToFrac dof1) (realToFrac dof2)

foreign import ccall "gsl_cdf_fdist_P" gsl_cdf_fdist_P :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble -> FCT.CDouble
gslCdfFdistP :: Double -- ^ x
             -> Double -- ^ nu1
             -> Double -- ^ nu2
             -> Double -- ^ P(x)
gslCdfFdistP x dof1 dof2 = realToFrac $ gsl_cdf_fdist_P (realToFrac x) (realToFrac dof1) (realToFrac dof2)

foreign import ccall "gsl_cdf_fdist_Q" gsl_cdf_fdist_Q :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble -> FCT.CDouble
gslCdfFdistQ :: Double -- ^ x
             -> Double -- ^ nu1
             -> Double -- ^ nu2
             -> Double -- ^ Q(x)
gslCdfFdistQ x dof1 dof2 = realToFrac $ gsl_cdf_fdist_Q (realToFrac x) (realToFrac dof1) (realToFrac dof2)

foreign import ccall "gsl_cdf_fdist_Pinv" gsl_cdf_fdist_Pinv :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble -> FCT.CDouble
gslCdfFdistPinv :: Double -- ^ P(x)
                -> Double -- ^ nu1
                -> Double -- ^ nu2
                -> Double -- ^ x
gslCdfFdistPinv pVal dof1 dof2 = realToFrac $ gsl_cdf_fdist_Pinv (realToFrac pVal) (realToFrac dof1) (realToFrac dof2)

foreign import ccall "gsl_cdf_fdist_Qinv" gsl_cdf_fdist_Qinv :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble -> FCT.CDouble
gslCdfFdistQinv :: Double -- ^ Q(x)
                -> Double -- ^ nu1
                -> Double -- ^ nu2
                -> Double -- ^ x
gslCdfFdistQinv qVal dof1 dof2 = realToFrac $ gsl_cdf_fdist_Qinv (realToFrac qVal) (realToFrac dof1) (realToFrac dof2)



{- p340 20.19 -- Student-t distribution -}
foreign import ccall "gsl_ran_tdist" gsl_ran_tdist :: FP.Ptr () -> FCT.CDouble -> IO FCT.CDouble
gslRanTdist :: RNG.GSLRng 
            -> Double -- ^ d.o.f nu
            -> IO Double
gslRanTdist (RNG.ToRng cPtr) dof = return.realToFrac =<< F.withForeignPtr cPtr (flip gsl_ran_tdist $ realToFrac dof)

foreign import ccall "gsl_ran_tdist_pdf" gsl_ran_tdist_pdf :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble
gslRanTdistPdf :: Double -- ^ x
               -> Double -- ^ nu
               -> Double -- ^ p(x)
gslRanTdistPdf x dof = realToFrac $ gsl_ran_tdist_pdf (realToFrac x) (realToFrac dof)

foreign import ccall "gsl_cdf_tdist_P" gsl_cdf_tdist_P :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble
gslCdfTdistP :: Double -- ^ x
             -> Double -- ^ nu
             -> Double -- ^ P(x)
gslCdfTdistP x dof = realToFrac $ gsl_cdf_tdist_P (realToFrac x) (realToFrac dof)

foreign import ccall "gsl_cdf_tdist_Q" gsl_cdf_tdist_Q :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble
gslCdfTdistQ :: Double -- ^ x
             -> Double -- ^ nu
             -> Double -- ^ Q(x)
gslCdfTdistQ x dof = realToFrac $ gsl_cdf_tdist_Q (realToFrac x) (realToFrac dof)

foreign import ccall "gsl_cdf_tdist_Pinv" gsl_cdf_tdist_Pinv :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble
gslCdfTdistPinv :: Double -- ^ P(x)
                -> Double -- ^ nu
                -> Double -- ^ x
gslCdfTdistPinv pVal dof = realToFrac $ gsl_cdf_tdist_Pinv (realToFrac pVal) (realToFrac dof)

foreign import ccall "gsl_cdf_tdist_Qinv" gsl_cdf_tdist_Qinv :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble
gslCdfTdistQinv :: Double -- ^ Q(x)
                -> Double -- ^ nu
                -> Double -- ^ x
gslCdfTdistQinv qVal dof = realToFrac $ gsl_cdf_tdist_Qinv (realToFrac qVal) (realToFrac dof)



{- p360 20.29 -- Poisson distribution -}
foreign import ccall "gsl_ran_poisson" gsl_ran_poisson :: FP.Ptr () -> FCT.CDouble -> IO FCT.CDouble
gslRanPoisson :: RNG.GSLRng 
              -> Double -- ^ mean
              -> IO Double
gslRanPoisson (RNG.ToRng cPtr) mu = return.realToFrac =<< F.withForeignPtr cPtr (flip gsl_ran_poisson $ realToFrac mu)

foreign import ccall "gsl_ran_poisson_pdf" gsl_ran_poisson_pdf :: FCT.CUInt -> FCT.CDouble -> FCT.CDouble
gslRanPoissonPdf :: Int -- ^ k
                 -> Double -- ^ mean
                 -> Double -- ^ p(k)
gslRanPoissonPdf k mu = realToFrac $ gsl_ran_poisson_pdf (fromIntegral k) (realToFrac mu)

foreign import ccall "gsl_cdf_poisson_P" gsl_cdf_poisson_P :: FCT.CUInt -> FCT.CDouble -> FCT.CDouble
gslCdfPoissonP :: Int -- ^ k
               -> Double -- ^ mean
               -> Double -- ^ P(k)
gslCdfPoissonP k mu = realToFrac $ gsl_cdf_poisson_P (fromIntegral k) (realToFrac mu)

foreign import ccall "gsl_cdf_poisson_Q" gsl_cdf_poisson_Q :: FCT.CUInt -> FCT.CDouble -> FCT.CDouble
gslCdfPoissonQ :: Int -- ^ k
               -> Double -- ^ mean
               -> Double -- ^ Q(k)
gslCdfPoissonQ k mu = realToFrac $ gsl_cdf_poisson_Q (fromIntegral k) (realToFrac mu)

