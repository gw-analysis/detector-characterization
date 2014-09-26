module HasKAL.TimeUtils.Function
( formatGPS
, fromGPS
) where
import HasKAL.TimeUtils.Signature

formatGPS :: Double -> GPSTIME
formatGPS x = (truncate x, truncate ((x-fromIntegral (truncate x))*1E9))

fromGPS :: GPSTIME -> Double
fromGPS (x, y) = (fromIntegral x) + 1E-9 * (fromIntegral y)
