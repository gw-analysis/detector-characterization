{-******************************************
  *     File Name: MKSA.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/12/21 19:21:08
  *******************************************-}

-- Reference
---- [1] ``GSL Reference Manual'' http://www.gnu.org/software/gsl/manual/html_node/
---- [2] "gsl/gsl_const_mksa.h"

{-# LANGUAGE ForeignFunctionInterface, CApiFFI #-}

module HasKAL.Constant.MKSA (
   hasKAL_const_mksa_speed_of_light
  ,hasKAL_const_mksa_gravitational_constant
  ,hasKAL_const_mksa_light_year
  ,hasKAL_const_mksa_parsec
  ,hasKAL_const_mksa_solar_mass
) where

import Foreign.C.Types

-- Speed of light [m/s]
hasKAL_const_mksa_speed_of_light :: Double
hasKAL_const_mksa_speed_of_light = realToFrac c_const_mksa_speed_of_light

-- gravitational constant [m^3/kg/s^2]
hasKAL_const_mksa_gravitational_constant :: Double
hasKAL_const_mksa_gravitational_constant = realToFrac c_const_mksa_gravitational_constant

-- 1 light year [m]
hasKAL_const_mksa_light_year :: Double
hasKAL_const_mksa_light_year = realToFrac c_const_mksa_light_year

-- 1 parsec [m]
hasKAL_const_mksa_parsec :: Double
hasKAL_const_mksa_parsec = realToFrac c_const_mksa_parsec

-- solor mass [kg]
hasKAL_const_mksa_solar_mass :: Double
hasKAL_const_mksa_solar_mass = realToFrac c_const_mksa_solar_mass

foreign import capi "gsl/gsl_const_mksa.h value GSL_CONST_MKSA_SPEED_OF_LIGHT" c_const_mksa_speed_of_light :: CDouble
foreign import capi "gsl/gsl_const_mksa.h value GSL_CONST_MKSA_GRAVITATIONAL_CONSTANT" c_const_mksa_gravitational_constant :: CDouble
foreign import capi "gsl/gsl_const_mksa.h value GSL_CONST_MKSA_LIGHT_YEAR" c_const_mksa_light_year :: CDouble
foreign import capi "gsl/gsl_const_mksa.h value GSL_CONST_MKSA_PARSEC" c_const_mksa_parsec :: CDouble
foreign import capi "gsl/gsl_const_mksa.h value GSL_CONST_MKSA_SOLAR_MASS" c_const_mksa_solar_mass :: CDouble

