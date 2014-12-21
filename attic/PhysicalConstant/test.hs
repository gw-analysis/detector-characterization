{-******************************************
  *     File Name: test.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/12/21 19:29:40
  *******************************************-}

import HasKAL.Constant
-- import HasKAL.Constant.MKSA
-- import HasKAL.Constant.CGSM

main = do
  putStrLn "###  speed of light [m/s], [cm/s]  ###"
  print $ hasKAL_const_mksa_speed_of_light
  print $ hasKAL_const_cgsm_speed_of_light
  putStrLn "\n###  gravitational constant [m^3/kg/s^2], [cm^3/g/s^2]  ###"
  print $ hasKAL_const_mksa_gravitational_constant
  print $ hasKAL_const_cgsm_gravitational_constant
  putStrLn "\n###  1 light year [m], [cm]  ###"
  print $ hasKAL_const_mksa_light_year
  print $ hasKAL_const_cgsm_light_year
  putStrLn "\n###  1 parsec [m], [cm]  ###"
  print $ hasKAL_const_mksa_parsec
  print $ hasKAL_const_cgsm_parsec
  putStrLn "\n###  solor mass [kg], [g]  ###"
  print $ hasKAL_const_mksa_solar_mass
  print $ hasKAL_const_cgsm_solar_mass
