{-******************************************
  *     File Name: SpecialFunctions.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/05/27 19:38:44
  *******************************************-}

module WrapGSL.SpecialFunctions (
       gslSfGamma
) where

import qualified Foreign.C.Types as FCT

-- p.84 7.19
foreign import ccall "gsl_sf_gamma" gsl_sf_gamma :: FCT.CDouble -> FCT.CDouble

gslSfGamma :: Double -> Double
gslSfGamma x = realToFrac $ gsl_sf_gamma $ realToFrac x

