{-
- Module for software injection
-}

module HasKAL.SimulationUtils.Injection.Function
( injDetectorResponse
--,
) where

import HasKAL.DetectorUtils.Detector
import HasKAL.DetectorUtils.DetectorParam
import HasKAL.DetectorUtils.Function
import HasKAL.DetectorUtils.Signature
import System.IO.Unsafe
import HasKAL.TimeUtils.Signature
import HasKAL.SimulationUtils.Injection.Signature



getPolarizations:: SOURCE_TYPE -> GravitationalWave
getPolarizations srcType
  | sigType srcType == "SG235Q8d9" =
    ( map (\x -> read x :: Double) (lines $ unsafePerformIO $ readFile "SG235Q8d9.txt")
    , map (\x -> read x :: Double) (lines $ unsafePerformIO $ readFile "SG235Q8d9.txt"))
  | otherwise = error "not recognized"


injDetectorResponse :: Detector -> SOURCE_TYPE -> GPSTIME -> [(GPSTIME, Double)]
injDetectorResponse detName srcType gps = do
  let detparam
        | detName == LIGO_Hanford = ligoHanford
        | detName == LIGO_Livingston = ligoLivingston
        | detName == KAGRA = kagra
        | otherwise = error "not recognized"

      (antennaPattern, tauS) = fplusfcrossts detparam (longitude srcType) (latitude srcType) (psi srcType)

      detresp = genDetectorResponse antennaPattern $ getPolarizations srcType

      startGPSTime = fromIntegral (fst gps) + 1E-9 * fromIntegral (snd gps) + tauS
      gpsTime = [startGPSTime+dt|dt<-[0, 1/(fs srcType)..]] :: [Double]
      timetrain = map (\x -> (truncate x,  truncate ((x-fromIntegral (truncate x))*1E9))) gpsTime
  zip timetrain detresp


