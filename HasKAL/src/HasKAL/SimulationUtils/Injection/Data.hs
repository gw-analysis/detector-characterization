module HasKAL.SimulationUtils.Injection.Data
where

import HasKAL.SimulationUtils.Injection.Sigunature

data SOURCE_TYPE = SOURCE_TYPE
       {  sigType :: SigType
       , longitude :: Double
       , latitude :: Double
       , psi :: Double
       , fs :: Double
       } deriving (Show)
