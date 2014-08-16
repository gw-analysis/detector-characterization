{-# LANGUAGE TemplateHaskell #-}

module HasKAL.DetectorUtils.Signature where

import HasKAL.DetectorUtils.Detector
import HasKAL.TimeUtils.Signature
import Control.Lens


type AntennaPattern = (Fplus,   Fcross)
type Fplus = Double
type Fcross= Double
type GravitationalWave = (Hplus,  Hcross)
type Hplus = [Double]
type Hcross = [Double]


data GWDATA = GWDATA { _detector :: Detector
                     , _dataType :: String
                     , _samplingFrequency :: Double
                     , _startGPSTime :: GPSTIME
                     , _stopGPSTime  :: GPSTIME
                     , _gwdata :: [Double]
                     } deriving (Show, Eq, Read)
$(makeLenses ''GWDATA)

