
module HasKAL.WaveUtils.Data
( WaveData (..)
, WaveProperty (..)
, mkWaveData
, mkLIGOHanfordWaveData
, getWaveProperty
) where


import HasKAL.DetectorUtils.Detector
import HasKAL.TimeUtils.Signature
import HasKAL.WaveUtils.Signature
import Numeric.LinearAlgebra


data WaveData = WaveData
  { detector :: Detector
  , dataType :: String
  , samplingFrequency :: Double
  , startGPSTime :: GPSTIME
  , stopGPSTime  :: GPSTIME
  , gwdata :: TimeSeries
  } deriving (Show, Eq, Read)


data WaveProperty = WaveProperty
  { mean :: Vector Double -> Double
  , variance :: Vector Double -> Double
  , spectrum :: Vector Double -> Double -> [(Double, Double)]
  , spectrogram :: Vector Double -> Double -> [(Double, Double, Double)]
  }

mkWaveData :: Detector -> String -> Double -> GPSTIME -> GPSTIME -> TimeSeries -> WaveData
mkWaveData det datatype fs startGPS stopGPS xs
  = WaveData { detector = det
             , dataType = datatype
             , samplingFrequency = fs
             , startGPSTime = startGPS
             , stopGPSTime = stopGPS
             , gwdata = xs
             }


mkLIGOHanfordWaveData :: String -> Double -> GPSTIME -> GPSTIME -> TimeSeries -> WaveData
mkLIGOHanfordWaveData datatype fs startGPS stopGPS xs
  = WaveData { detector = LIGO_Hanford
             , dataType = datatype
             , samplingFrequency = fs
             , startGPSTime = startGPS
             , stopGPSTime = stopGPS
             , gwdata = xs
             }


getWaveProperty :: WaveData -> WaveProperty
getWaveProperty x = undefined


updateWaveDatagwdata :: WaveData -> TimeSeries -> Maybe WaveData
updateWaveDatagwdata v w
  | dim (gwdata v)==dim w
    = Just $ mkWaveData (detector v) (dataType v) (samplingFrequency v) (startGPSTime v) (stopGPSTime v) w
  | otherwise = Nothing
