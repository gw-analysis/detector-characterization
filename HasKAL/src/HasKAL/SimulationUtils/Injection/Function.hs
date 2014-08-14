{-
- Module for software injection
-}

module HasKAL.SimulationUtils.Injection.Function
( injDetectorResponse
--,
) where

import HasKAL.DetectorUtils
import System.IO.Unsafe
import HasKAL.TimeUtils.Signature
import HasKAL.SimulationUtils.Injection.Signature
import HasKAL.Misc.Environment (haskalOpt)
import System.FilePath (pathSeparator)

getPolarizations:: SOURCE_TYPE -> GravitationalWave
getPolarizations srcType
  | sigType srcType == "SG235Q8d9" =
    (
    map (\x -> read x :: Double) dat
    , replicate (length dat) 0
    )
  | otherwise = error "not recognized"
  where
    dat = lines $ unsafePerformIO $ readFile
      (haskalOpt ++ [pathSeparator] ++ "MockDataChallenge" ++ [pathSeparator]
      ++ "Waveforms" ++ [pathSeparator] ++ (sigType srcType) ++ ".txt")


injDetectorResponse :: Detector -> SOURCE_TYPE -> GPSTIME -> [(GPSTIME, Double)]
injDetectorResponse detName srcType gps = do
  let detparam
        | detName == LIGO_Hanford = ligoHanford
        | detName == LIGO_Livingston = ligoLivingston
        | detName == KAGRA = kagra
        | otherwise = error "not recognized"

      (antennaPattern, tauS) =
        fplusfcrossts detparam (longitude srcType) (latitude srcType) (psi srcType)

      detresp = genDetectorResponse antennaPattern $ getPolarizations srcType

      startGPSTime = fromIntegral (fst gps) + 1E-9 * fromIntegral (snd gps) + tauS
      gpsTime = [startGPSTime+dt|dt<-[0, 1/(fs srcType)..]] :: [Double]
      timetrain = map (\x -> (truncate x, truncate ((x-fromIntegral (truncate x))*1E9))) gpsTime
  zip timetrain detresp


