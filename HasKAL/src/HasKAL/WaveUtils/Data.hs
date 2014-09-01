
module HasKAL.WaveUtils.Data 
where

import HasKAL.DetectorUtils.Detector
import HasKAL.TimeUtils.Signature


data WaveData = WaveData { 
    detector :: Detector
  , dataType :: String
  , samplingFrequency :: Double
  , startGPSTime :: GPSTIME
  , stopGPSTime  :: GPSTIME
  , gwdata :: [Double]} 
  deriving (Show, Eq, Read)



