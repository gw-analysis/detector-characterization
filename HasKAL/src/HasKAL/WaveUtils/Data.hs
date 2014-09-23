
module HasKAL.WaveUtils.Data
where

import HasKAL.DetectorUtils.Detector
import HasKAL.TimeUtils.Signature
import Numeric.LinearAlgebra

data WaveData = WaveData {
    detector :: Detector
  , dataType :: String
  , samplingFrequency :: Double
  , startGPSTime :: GPSTIME
  , stopGPSTime  :: GPSTIME
  , gwdata :: TimeSeries}
  deriving (Show, Eq, Read)

data WaveProperty = WaveProperty {
    mean :: Vector Double -> Double
  , variance :: Vector Double -> Double
  , spectrum :: Vector Double -> Double -> [(Double, Double)]
  , spectrogram :: Vector Double -> Double -> [(Double, Double, Double)]
    }
    deriving (Show, Eq, Read)


mkLIGOdataStructure :: String -> Double -> GPSTIME -> GPSTIME -> TimeSeries -> WaveData
mkLIGOdataStructure datatype fs startGPS stopGPS xs
  = WaveData { LIGO_Hanford
             , datatype
             , fs
             , startGPS
             , stopGPS
             xs}


getWaveProperty :: WaveData -> WaveProperty
getWaveProperty x = undefined


