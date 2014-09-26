{-
- Module for software injection
-}


module HasKAL.SimulationUtils.Injection.Function
( injDetectorResponse
, doInjection
) where

import System.FilePath ((</>))
import System.Directory (doesFileExist)
import System.IO.Unsafe
import Control.Monad ()
import Numeric.LinearAlgebra

import HasKAL.DetectorUtils
import HasKAL.Misc.Environment (haskalOpt)
import HasKAL.SimulationUtils.Injection.Signature
import HasKAL.SimulationUtils.Injection.Data
import HasKAL.TimeUtils.Signature
import HasKAL.TimeUtils.Function (formatGPS)
import HasKAL.WaveUtils.Data
import HasKAL.WaveUtils.Signature


getPolarizations:: SOURCE_TYPE -> GravitationalWave
getPolarizations srcType = unsafePerformIO $ do
  doesFileExist mdcFilePath  >>= \y ->
    case y of True -> return (fromList (map (((hrss srcType)*).(\x -> read x :: Double)) dat), fromList (replicate (length dat) (0::Double)))
              False -> error "not recognized"
      where
        mdcFilePath = haskalOpt </> "MockDataChallenge" </> "Waveforms" </> (sigType srcType)
        dat = lines $ unsafePerformIO $ readFile mdcFilePath


injDetectorResponse :: Detector -> SOURCE_TYPE -> GPSTIME -> WaveData
injDetectorResponse detName srcType gps = do
  let detparam
        | detName == LIGO_Hanford = ligoHanford
        | detName == LIGO_Livingston = ligoLivingston
        | detName == KAGRA = kagra
        | otherwise = error "not recognized"

      (antennaPattern, tauS) =
        fplusfcrossts detparam (longitude srcType) (latitude srcType) (psi srcType)

      detresp = genDetectorResponse antennaPattern $ getPolarizations srcType

      startGPSTime' = fromIntegral (fst gps) + 1E-9 * fromIntegral (snd gps) + tauS
  WaveData { detector = detName
           , dataType = "SoftwareInjection"
           , samplingFrequency = fs srcType
           , startGPSTime = formatGPS startGPSTime'
           , stopGPSTime  = formatGPS $ startGPSTime'+(fromIntegral (dim detresp)-1)/(fs srcType)
           , gwdata = detresp
           }


doInjection :: WaveData -> WaveData -> WaveData
doInjection dat injdat = unsafePerformIO $ do
  let tdat = fromIntegral (fst (startGPSTime dat))
        + 1E-9 * fromIntegral (snd (startGPSTime dat))::Double
      tinjdat = fromIntegral (fst (startGPSTime injdat))
        + 1E-9 * fromIntegral (snd (startGPSTime injdat))::Double
      timeSlide = floor $ (tinjdat - tdat)*(samplingFrequency dat)
      newdat
--        | timeSlide < 0 = vjoin [(subVector (timeSlide-1) nlen1 vinjdata + subVector 0 nlen1 vdata)
--                               , subVector (nlen1-1) (nvinjdata-nlen1)]
        | timeSlide >=0&&timeSlide<=nvdata-nvinjdata
            = join [subVector 0 timeSlide vdata
                   , subVector (timeSlide-1) nvinjdata vdata + vinjdata
                   , subVector (timeSlide+nvinjdata-1) (nvdata - nvinjdata - timeSlide) vdata]
        | otherwise = error "Injection not succeeded"
          where nlen1 = nvinjdata - timeSlide
                nvinjdata = dim vinjdata
                nvdata = dim vdata
                vinjdata = gwdata injdat :: Vector Double
                vdata = gwdata dat  :: Vector Double
      newGWData = dat
      gwdata newGWData = newdat
  return newGWData

