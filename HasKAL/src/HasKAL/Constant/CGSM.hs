{-# HADDOCK Markdown #-}
{- |
Module      : HasKAL.Constant.CGSM
Description : This is documentation tests.
Copyright   : (c) WhoAmI, 2014
License     : ???
Maintainer  : hoge@hoge.com
Stability   : test
Portability : POSIX

Physical constant in CGS unit

- [1] "GSL Reference Manual"  http://www.gnu.org/software/gsl/manual/html_node/
- [2] "gsl/gsl_const_cgsm.h"
-}

{-# LANGUAGE ForeignFunctionInterface, CApiFFI #-}

module HasKAL.Constant.CGSM (
   hasKAL_const_cgsm_speed_of_light
  ,hasKAL_const_cgsm_gravitational_constant
  ,hasKAL_const_cgsm_light_year
  ,hasKAL_const_cgsm_parsec
  ,hasKAL_const_cgsm_solar_mass
) where

import Foreign.C.Types

-- | Speed of light [cm/s]
hasKAL_const_cgsm_speed_of_light :: Double
hasKAL_const_cgsm_speed_of_light = realToFrac c_const_cgsm_speed_of_light

-- | gravitational constant [cm^3/g/s^2]
hasKAL_const_cgsm_gravitational_constant :: Double
hasKAL_const_cgsm_gravitational_constant = realToFrac c_const_cgsm_gravitational_constant

-- | 1 light year [cm]
hasKAL_const_cgsm_light_year :: Double
hasKAL_const_cgsm_light_year = realToFrac c_const_cgsm_light_year

-- | 1 parsec [cm]
hasKAL_const_cgsm_parsec :: Double
hasKAL_const_cgsm_parsec = realToFrac c_const_cgsm_parsec

-- | solor mass [g]
hasKAL_const_cgsm_solar_mass :: Double
hasKAL_const_cgsm_solar_mass = realToFrac c_const_cgsm_solar_mass

foreign import capi "gsl/gsl_const_cgsm.h value GSL_CONST_CGSM_SPEED_OF_LIGHT" c_const_cgsm_speed_of_light :: CDouble
foreign import capi "gsl/gsl_const_cgsm.h value GSL_CONST_CGSM_GRAVITATIONAL_CONSTANT" c_const_cgsm_gravitational_constant :: CDouble
foreign import capi "gsl/gsl_const_cgsm.h value GSL_CONST_CGSM_LIGHT_YEAR" c_const_cgsm_light_year :: CDouble
foreign import capi "gsl/gsl_const_cgsm.h value GSL_CONST_CGSM_PARSEC" c_const_cgsm_parsec :: CDouble
foreign import capi "gsl/gsl_const_cgsm.h value GSL_CONST_CGSM_SOLAR_MASS" c_const_cgsm_solar_mass :: CDouble

