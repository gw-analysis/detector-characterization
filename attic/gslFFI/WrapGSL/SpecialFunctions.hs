{-******************************************
  *     File Name: SpecialFunctions.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/06/10 18:15:17
  *******************************************-}

module WrapGSL.SpecialFunctions (
        gslSfGamma
       ,gslSfPsi
) where

import qualified Foreign.C.Types as FCT

-- p.84 7.19
foreign import ccall "gsl_sf_gamma" gsl_sf_gamma :: FCT.CDouble -> FCT.CDouble

-- p.108 7.28.1
foreign import ccall "gsl_sf_psi" gsl_sf_psi :: FCT.CDouble -> FCT.CDouble


gslSfGamma :: Double -> Double
gslSfGamma x = realToFrac $ gsl_sf_gamma $ realToFrac x

gslSfPsi :: Double -> Double
gslSfPsi x = realToFrac $ gsl_sf_psi $ realToFrac x
