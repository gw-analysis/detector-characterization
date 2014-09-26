module HasKAL.TimeUtils.Function
( formatGPS
, deformatGPS
) where
import HasKAL.TimeUtils.Signature

formatGPS :: Double -> GPSTIME
formatGPS x = (truncate x, truncate ((x-fromIntegral (truncate x))*1E9))

deformatGPS :: GPSTIME -> Double
deformatGPS (x, y) = (fromIntegral x) + 1E-9 * (fromIntegral y)
