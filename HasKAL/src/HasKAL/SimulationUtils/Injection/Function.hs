{-
- Module for software injection
-}



module HasKAL.SimulationUtils.Injection.Function
( injDetectorResponse
, doInjection
) where

import HasKAL.DetectorUtils
import HasKAL.Misc.Environment (haskalOpt)
import HasKAL.SimulationUtils.Injection.Signature
import HasKAL.TimeUtils.Signature
import HasKAL.TimeUtils.Function (formatGPS)

import System.FilePath ((</>))
import System.Directory (doesFileExist)
import Control.Monad ()
import System.IO.Unsafe
import Control.Lens
import Numeric.LinearAlgebra


getPolarizations:: SOURCE_TYPE -> GravitationalWave
getPolarizations srcType = unsafePerformIO $ do
  doesFileExist mdcFilePath  >>= \y ->
    case y of True -> return (map (\x -> read x :: Double) dat, replicate (length dat) 0)
              False -> error "not recognized"
      where
        mdcFilePath = haskalOpt </> "MockDataChallenge" </> "Waveforms" </> srcType^.sigType
        dat = lines $ unsafePerformIO $ readFile mdcFilePath


injDetectorResponse :: Detector -> SOURCE_TYPE -> GPSTIME -> GWDATA
injDetectorResponse detName srcType gps = do
  let detparam
        | detName == LIGO_Hanford = ligoHanford
        | detName == LIGO_Livingston = ligoLivingston
        | detName == KAGRA = kagra
        | otherwise = error "not recognized"

      (antennaPattern, tauS) =
        fplusfcrossts detparam (srcType^.longitude) (srcType^.latitude) (srcType^.psi)

      detresp = genDetectorResponse antennaPattern $ getPolarizations srcType

      startGPSTime' = fromIntegral (fst gps) + 1E-9 * fromIntegral (snd gps) + tauS
  GWDATA { _detector=detName
         , _dataType="SoftwareInjection"
         , _samplingFrequency=srcType^.fs
         , _startGPSTime = formatGPS startGPSTime'
         , _stopGPSTime  = formatGPS $ startGPSTime'+(fromIntegral (length detresp)-1)/(srcType^.fs)
         , _gwdata = detresp
         }


doInjection :: GWDATA -> GWDATA -> GWDATA
doInjection dat injdat = unsafePerformIO $ do
  let tdat = fromIntegral (dat^.(startGPSTime._1))
        + 1E-9 * fromIntegral (dat^.(startGPSTime._2))::Double
      tinjdat = fromIntegral (injdat^.(startGPSTime._1))
        + 1E-9 * fromIntegral (injdat^.(startGPSTime._2))::Double
      timeSlide = floor $ (tinjdat - tdat)*dat^.samplingFrequency
      newdat
--        | timeSlide < 0 = vjoin [(subVector (timeSlide-1) nlen1 vinjdata + subVector 0 nlen1 vdata)
--                               , subVector (nlen1-1) (nvinjdata-nlen1)]
        | timeSlide >=0&&timeSlide<=nvdata-nvinjdata
            = toList $ join [subVector 0 timeSlide vdata
                             , subVector (timeSlide-1) nvinjdata vdata + vinjdata
                             , subVector (timeSlide+nvinjdata-1) (nvdata - nvinjdata - timeSlide) vdata]
        | otherwise = error "Injection not succeeded"
          where nlen1 = nvinjdata - timeSlide
                nvinjdata = dim vinjdata
                nvdata = dim vdata
                vinjdata = fromList $ injdat^.gwdata
                vdata = fromList $ dat^.gwdata
      newGWData = dat
  return $ gwdata .~ newdat $ newGWData
  return newGWData

