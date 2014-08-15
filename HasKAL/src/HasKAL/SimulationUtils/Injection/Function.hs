{-
- Module for software injection
-}

module HasKAL.SimulationUtils.Injection.Function
( injDetectorResponse
--,
) where

import HasKAL.DetectorUtils
import HasKAL.Misc.Environment (haskalOpt)
import HasKAL.SimulationUtils.Injection.Signature
import HasKAL.TimeUtils.Signature
import HasKAL.TimeUtils.Function (formatGPS)

import System.FilePath ((</>))
import System.Directory (doesFileExist)
import Control.Monad ()
import System.IO.Unsafe()


getPolarizations:: SOURCE_TYPE -> GravitationalWave
getPolarizations srcType = unsafePerformIO $ do
  doesFileExist mdcFilePath  >>= \y ->
    case y of True -> return (map (\x -> read x :: Double) dat, replicate (length dat) 0)
              False -> error "not recognized"
      where
        mdcFilePath = haskalOpt </> "MockDataChallenge" </> "Waveforms" </> (sigType srcType)
        dat = lines $ unsafePerformIO $ readFile mdcFilePath


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
      timetrain = map formatGPS gpsTime
  zip timetrain detresp

