

module HasKAL.SimulationUtils.Injection.Data
where

import HasKAL.SimulationUtils.Injection.Signature


data SOURCE_TYPE = SOURCE_TYPE
  { sigType :: SigType
  , longitude :: Double
  , latitude :: Double
  , psi :: Double
  , fs :: Double
  , hrss :: Double
  } deriving (Show, Eq)


mkSOURCE_TYPE_S5 :: SigType -> Double -> Double -> Double -> Double -> SOURCE_TYPE
mkSOURCE_TYPE_S5 a b c d e =
  SOURCE_TYPE { sigType = a
              , longitude = b
              , latitude = c
              , psi = d
              , fs = 16384.0 -- inS5 case, sampling frequency is fixed
              , hrss = e
              }

