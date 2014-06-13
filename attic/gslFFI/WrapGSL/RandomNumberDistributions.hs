{-******************************************
  *     File Name: RandomNumberDistributions.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/06/10 15:03:12
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
              ,gslRanFdistPdf
              ,gslCdfFdistP
              ,gslCdfFdistQ
              ,gslCdfFdistPinv
              ,gslCdfFdistQinv
              ,gslRanTdistPdf
              ,gslCdfTdistP
              ,gslCdfTdistQ
              ,gslCdfTdistPinv
              ,gslCdfTdistQinv
              ,gslRanPoissonPdf
              ,gslCdfPoissonP
              ,gslCdfPoissonQ
) where

import qualified Foreign.C.Types as FCT

-- p.309 20.2
foreign import ccall "gsl_ran_gaussian_pdf" gsl_ran_gaussian_pdf :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble
foreign import ccall "gsl_cdf_gaussian_P" gsl_cdf_gaussian_P :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble
foreign import ccall "gsl_cdf_gaussian_Q" gsl_cdf_gaussian_Q :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble
foreign import ccall "gsl_cdf_gaussian_Pinv" gsl_cdf_gaussian_Pinv :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble
foreign import ccall "gsl_cdf_gaussian_Qinv" gsl_cdf_gaussian_Qinv :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble

-- p.324 20.9
foreign import ccall "gsl_ran_rayleigh_pdf" gsl_ran_rayleigh_pdf :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble
foreign import ccall "gsl_cdf_rayleigh_P" gsl_cdf_rayleigh_P :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble
foreign import ccall "gsl_cdf_rayleigh_Q" gsl_cdf_rayleigh_Q :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble
foreign import ccall "gsl_cdf_rayleigh_Pinv" gsl_cdf_rayleigh_Pinv :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble
foreign import ccall "gsl_cdf_rayleigh_Qinv" gsl_cdf_rayleigh_Qinv :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble

-- p336 20.17
foreign import ccall "gsl_ran_chisq_pdf" gsl_ran_chisq_pdf :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble
foreign import ccall "gsl_cdf_chisq_P" gsl_cdf_chisq_P :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble
foreign import ccall "gsl_cdf_chisq_Q" gsl_cdf_chisq_Q :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble
foreign import ccall "gsl_cdf_chisq_Pinv" gsl_cdf_chisq_Pinv :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble
foreign import ccall "gsl_cdf_chisq_Qinv" gsl_cdf_chisq_Qinv :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble

-- p338 20.18
foreign import ccall "gsl_ran_fdist_pdf" gsl_ran_fdist_pdf :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble -> FCT.CDouble
foreign import ccall "gsl_cdf_fdist_P" gsl_cdf_fdist_P :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble -> FCT.CDouble
foreign import ccall "gsl_cdf_fdist_Q" gsl_cdf_fdist_Q :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble -> FCT.CDouble
foreign import ccall "gsl_cdf_fdist_Pinv" gsl_cdf_fdist_Pinv :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble -> FCT.CDouble
foreign import ccall "gsl_cdf_fdist_Qinv" gsl_cdf_fdist_Qinv :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble -> FCT.CDouble

-- p340 20.19
foreign import ccall "gsl_ran_tdist_pdf" gsl_ran_tdist_pdf :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble
foreign import ccall "gsl_cdf_tdist_P" gsl_cdf_tdist_P :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble
foreign import ccall "gsl_cdf_tdist_Q" gsl_cdf_tdist_Q :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble
foreign import ccall "gsl_cdf_tdist_Pinv" gsl_cdf_tdist_Pinv :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble
foreign import ccall "gsl_cdf_tdist_Qinv" gsl_cdf_tdist_Qinv :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble

-- p360 20.29
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


{- F distribution -}
gslRanFdistPdf :: Double -> Double -> Double -> Double
gslRanFdistPdf x dof1 dof2  = realToFrac $ gsl_ran_fdist_pdf (realToFrac x) (realToFrac dof1) (realToFrac dof2)

gslCdfFdistP :: Double -> Double -> Double -> Double
gslCdfFdistP x dof1 dof2 = realToFrac $ gsl_cdf_fdist_P (realToFrac x) (realToFrac dof1) (realToFrac dof2)

gslCdfFdistQ :: Double -> Double -> Double -> Double
gslCdfFdistQ x dof1 dof2 = realToFrac $ gsl_cdf_fdist_Q (realToFrac x) (realToFrac dof1) (realToFrac dof2)

gslCdfFdistPinv :: Double -> Double -> Double -> Double
gslCdfFdistPinv pVal dof1 dof2 = realToFrac $ gsl_cdf_fdist_Pinv (realToFrac pVal) (realToFrac dof1) (realToFrac dof2)

gslCdfFdistQinv :: Double -> Double -> Double -> Double
gslCdfFdistQinv qVal dof1 dof2 = realToFrac $ gsl_cdf_fdist_Qinv (realToFrac qVal) (realToFrac dof1) (realToFrac dof2)


{- Student-t distribution -}
gslRanTdistPdf :: Double -> Double -> Double
gslRanTdistPdf x dof = realToFrac $ gsl_ran_tdist_pdf (realToFrac x) (realToFrac dof)

gslCdfTdistP :: Double -> Double -> Double
gslCdfTdistP x dof = realToFrac $ gsl_cdf_tdist_P (realToFrac x) (realToFrac dof)

gslCdfTdistQ :: Double -> Double -> Double
gslCdfTdistQ x dof = realToFrac $ gsl_cdf_tdist_Q (realToFrac x) (realToFrac dof)

gslCdfTdistPinv :: Double -> Double -> Double
gslCdfTdistPinv pVal dof = realToFrac $ gsl_cdf_tdist_Pinv (realToFrac pVal) (realToFrac dof)

gslCdfTdistQinv :: Double -> Double -> Double
gslCdfTdistQinv qVal dof = realToFrac $ gsl_cdf_tdist_Qinv (realToFrac qVal) (realToFrac dof)


{- Poisson distribution -}
gslRanPoissonPdf :: Int -> Double -> Double
gslRanPoissonPdf event mean = realToFrac $ gsl_ran_poisson_pdf (fromIntegral event) (realToFrac mean)

gslCdfPoissonP :: Int -> Double -> Double
gslCdfPoissonP event mean = realToFrac $ gsl_cdf_poisson_P (fromIntegral event) (realToFrac mean)

gslCdfPoissonQ :: Int -> Double -> Double
gslCdfPoissonQ event mean = realToFrac $ gsl_cdf_poisson_Q (fromIntegral event) (realToFrac mean)


