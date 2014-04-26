{-******************************************
  *     File Name: RandomNumberDistributions.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/04/12 19:31:11
  *******************************************-}
module WrapGSL.RandomNumberDistributions (
               gslRanGaussianPdf
              ,gslCdfGaussianP
              ,gslCdfGaussianQ
              ,gslCdfGaussianPinv
              ,gslCdfGaussianQinv
              ,gslRanRayleighPdf
              ,gslCdfRayleighP
              ,gslCdfRayleighQ
              ,gslCdfRayleighPinv
              ,gslCdfRayleighQinv
              ,gslRanChisqPdf
              ,gslCdfChisqP
              ,gslCdfChisqQ
              ,gslCdfChisqPinv
              ,gslCdfChisqQinv
              ,gslRanPoissonPdf
              ,gslCdfPoissonP
              ,gslCdfPoissonQ
) where

import qualified Foreign.C.Types as FCT

foreign import ccall "gsl_ran_gaussian_pdf" gsl_ran_gaussian_pdf :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble
foreign import ccall "gsl_cdf_gaussian_P" gsl_cdf_gaussian_P :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble
foreign import ccall "gsl_cdf_gaussian_Q" gsl_cdf_gaussian_Q :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble
foreign import ccall "gsl_cdf_gaussian_Pinv" gsl_cdf_gaussian_Pinv :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble
foreign import ccall "gsl_cdf_gaussian_Qinv" gsl_cdf_gaussian_Qinv :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble

foreign import ccall "gsl_ran_rayleigh_pdf" gsl_ran_rayleigh_pdf :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble
foreign import ccall "gsl_cdf_rayleigh_P" gsl_cdf_rayleigh_P :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble
foreign import ccall "gsl_cdf_rayleigh_Q" gsl_cdf_rayleigh_Q :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble
foreign import ccall "gsl_cdf_rayleigh_Pinv" gsl_cdf_rayleigh_Pinv :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble
foreign import ccall "gsl_cdf_rayleigh_Qinv" gsl_cdf_rayleigh_Qinv :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble

foreign import ccall "gsl_ran_chisq_pdf" gsl_ran_chisq_pdf :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble
foreign import ccall "gsl_cdf_chisq_P" gsl_cdf_chisq_P :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble
foreign import ccall "gsl_cdf_chisq_Q" gsl_cdf_chisq_Q :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble
foreign import ccall "gsl_cdf_chisq_Pinv" gsl_cdf_chisq_Pinv :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble
foreign import ccall "gsl_cdf_chisq_Qinv" gsl_cdf_chisq_Qinv :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble

foreign import ccall "gsl_ran_poisson_pdf" gsl_ran_poisson_pdf :: FCT.CUInt -> FCT.CDouble -> FCT.CDouble
foreign import ccall "gsl_cdf_poisson_P" gsl_cdf_poisson_P :: FCT.CUInt -> FCT.CDouble -> FCT.CDouble
foreign import ccall "gsl_cdf_poisson_Q" gsl_cdf_poisson_Q :: FCT.CUInt -> FCT.CDouble -> FCT.CDouble


{- Gaussian distribution -}
gslRanGaussianPdf :: Double -> Double -> Double
gslRanGaussianPdf x sigma = realToFrac $ gsl_ran_gaussian_pdf (realToFrac x) (realToFrac sigma)

gslCdfGaussianP :: Double -> Double -> Double
gslCdfGaussianP x sigma = realToFrac $ gsl_cdf_gaussian_P (realToFrac x) (realToFrac sigma)

gslCdfGaussianQ :: Double -> Double -> Double
gslCdfGaussianQ x sigma = realToFrac $ gsl_cdf_gaussian_Q (realToFrac x) (realToFrac sigma)

gslCdfGaussianPinv :: Double -> Double -> Double
gslCdfGaussianPinv pVal sigma = realToFrac $ gsl_cdf_gaussian_Pinv (realToFrac pVal) (realToFrac sigma)

gslCdfGaussianQinv :: Double -> Double -> Double
gslCdfGaussianQinv qVal sigma = realToFrac $ gsl_cdf_gaussian_Qinv (realToFrac qVal) (realToFrac sigma)


{- Rayleigh distribution -}
gslRanRayleighPdf :: Double -> Double -> Double
gslRanRayleighPdf x sigma = realToFrac $ gsl_ran_rayleigh_pdf (realToFrac x) (realToFrac sigma)

gslCdfRayleighP :: Double -> Double -> Double
gslCdfRayleighP x sigma = realToFrac $ gsl_cdf_rayleigh_P (realToFrac x) (realToFrac sigma)

gslCdfRayleighQ :: Double -> Double -> Double
gslCdfRayleighQ x sigma = realToFrac $ gsl_cdf_rayleigh_Q (realToFrac x) (realToFrac sigma)

gslCdfRayleighPinv :: Double -> Double -> Double
gslCdfRayleighPinv pVal sigma = realToFrac $ gsl_cdf_rayleigh_Pinv (realToFrac pVal) (realToFrac sigma)

gslCdfRayleighQinv :: Double -> Double -> Double
gslCdfRayleighQinv qVal sigma = realToFrac $ gsl_cdf_rayleigh_Qinv (realToFrac qVal) (realToFrac sigma)


{- Chi-Squared distribution -}
gslRanChisqPdf :: Double -> Double -> Double
gslRanChisqPdf x dof = realToFrac $ gsl_ran_chisq_pdf (realToFrac x) (realToFrac dof)

gslCdfChisqP :: Double -> Double -> Double
gslCdfChisqP x dof = realToFrac $ gsl_cdf_chisq_P (realToFrac x) (realToFrac dof)

gslCdfChisqQ :: Double -> Double -> Double
gslCdfChisqQ x dof = realToFrac $ gsl_cdf_chisq_Q (realToFrac x) (realToFrac dof)

gslCdfChisqPinv :: Double -> Double -> Double
gslCdfChisqPinv pVal dof = realToFrac $ gsl_cdf_chisq_Pinv (realToFrac pVal) (realToFrac dof)

gslCdfChisqQinv :: Double -> Double -> Double
gslCdfChisqQinv qVal dof = realToFrac $ gsl_cdf_chisq_Qinv (realToFrac qVal) (realToFrac dof)


{- Poisson distribution -}
gslRanPoissonPdf :: Int -> Double -> Double
gslRanPoissonPdf event mean = realToFrac $ gsl_ran_poisson_pdf (fromIntegral event) (realToFrac mean)

gslCdfPoissonP :: Int -> Double -> Double
gslCdfPoissonP event mean = realToFrac $ gsl_cdf_poisson_P (fromIntegral event) (realToFrac mean)

gslCdfPoissonQ :: Int -> Double -> Double
gslCdfPoissonQ event mean = realToFrac $ gsl_cdf_poisson_Q (fromIntegral event) (realToFrac mean)


