
module HasKAL.WaveUtils.Data
( WaveData (..)
, WaveProperty (..)
, mkWaveData
, mkLIGOHanfordWaveData
, getWaveProperty
, updateWaveDatagwdata
, dropWaveData
, takeWaveData
) where


import HasKAL.DetectorUtils.Detector
import HasKAL.TimeUtils.Signature
import HasKAL.TimeUtils.Function
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


dropWaveData :: Int -> WaveData -> WaveData
dropWaveData n x = do
  let n' = max n 0
  let t = (fromIntegral n') / (samplingFrequency x)
      newstartGPSTime = formatGPS $ deformatGPS (startGPSTime x) + t
      newgwdata = subVector n' (dim (gwdata x) - n') (gwdata x)
  mkWaveData (detector x) (dataType x) (samplingFrequency x) newstartGPSTime (stopGPSTime x) newgwdata


takeWaveData :: Int -> WaveData -> WaveData
takeWaveData n x = do
  let n' = max n 0
  let t = (fromIntegral n') / (samplingFrequency x)
      newstopGPSTime = formatGPS $ deformatGPS (startGPSTime x) + t
      newgwdata = subVector 0 n' (gwdata x)
  mkWaveData (detector x) (dataType x) (samplingFrequency x) (startGPSTime x) newstopGPSTime newgwdata





