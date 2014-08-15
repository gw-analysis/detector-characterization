module HasKAL.TimeUtils.Function
( formatGPS
--,
) where
import HasKAL.TimeUtils.Function

formatGPS :: Double -> GPSTIME
formatGPS x = (truncate x, truncate ((x-fromIntegral (truncate x))*1E9))
