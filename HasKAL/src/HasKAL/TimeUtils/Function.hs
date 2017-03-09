module HasKAL.TimeUtils.Function
( module HasKAL.TimeUtils.Signature
, formatGPS
, deformatGPS
, diffGPS
, addGPS
) where

  
import HasKAL.TimeUtils.Signature

formatGPS :: Double -> GPSTIME
formatGPS x = (truncate x, truncate ((x-fromIntegral (truncate x))*1E9))

deformatGPS :: GPSTIME -> Double
deformatGPS (x, y) = (fromIntegral x) + 1E-9 * (fromIntegral y)

diffGPS :: GPSTIME -> GPSTIME -> GPSTIME
diffGPS (s1, n1) (s2, n2)
  | n1 >= n2  = (s1-s2, n1-n2)
  | otherwise = (s1-s2-1, n2-n1)

addGPS :: GPSTIME -> GPSTIME -> GPSTIME
addGPS (s1, n1) (s2, n2)
  | sumN < 1000000000 = (s1+s2, sumN)
  | otherwise         = (s1+s2+1, sumN-1000000000)
  where sumN = n1 + n2
