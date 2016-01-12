{-
- Module for software injection
-}


module HasKAL.SimulationUtils.Injection.Function
( injDetectorResponse
, doInjection
, doInjection'
) where

import Control.DeepSeq (deepseq)
import Control.Monad ()
import Control.Monad.ST(ST)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Vector.Storable as V
import Data.Packed.ST
import Numeric.LinearAlgebra
import System.FilePath ((</>))
import System.Directory (doesFileExist)
import System.IO.Unsafe (unsafePerformIO)

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
    case y of
      True -> do
        let hp' = fromList (map (\x -> read x :: Double) dat)
            hc = fromList (replicate (length dat) (0::Double))
            hp = scale ((hrss srcType)/(norm2 hp')) hp'
        return (hp,hc)
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
doInjection dat injdat = do
  let tdat = fromIntegral (fst (startGPSTime dat))
        + 1E-9 * fromIntegral (snd (startGPSTime dat))::Double
      tinjdat = fromIntegral (fst (startGPSTime injdat))
        + 1E-9 * fromIntegral (snd (startGPSTime injdat))::Double
      timeSlide = floor $ (tinjdat - tdat)*(samplingFrequency dat)

  let newdat
--        | timeSlide < 0 = vjoin [(subVector (timeSlide-1) nlen1 vinjdata + subVector 0 nlen1 vdata)
--                               , subVector (nlen1-1) (nvinjdata-nlen1)]
        | timeSlide >=0&&timeSlide<=nvdata-nvinjdata
            = V.concat [subVector 0 timeSlide vdata
                   , add (subVector timeSlide nvinjdata vdata) vinjdata
                   , subVector (timeSlide+nvinjdata-1) (nvdata - nvinjdata - timeSlide) vdata]
        | otherwise = error "Injection not succeeded"
          where nlen1 = nvinjdata - timeSlide
                nvinjdata = dim vinjdata
                nvdata = dim vdata
                vinjdata = gwdata injdat :: Vector Double
                vdata = gwdata dat  :: Vector Double
  mkWaveData (detector dat) (dataType dat) (samplingFrequency dat) (startGPSTime dat) (stopGPSTime dat) newdat


doInjection' :: WaveData -> WaveData -> WaveData
doInjection' dat injdat
  | timeSlide >=0&&timeSlide<=nvdata-nvinjdata
      = unsafePerformIO $ do
          deepseq (addInjsig timeSlide (gwdata dat) vinjdata) return ()
          return dat
  | otherwise = error "Injection not succeeded"
  where
--    vdata     = gwdata dat  :: Vector Double
    nvdata    = dim (gwdata dat)
    vinjdata  = gwdata injdat :: Vector Double
    nvinjdata = dim vinjdata
    tdat = fromIntegral (fst (startGPSTime dat))
         + 1E-9 * fromIntegral (snd (startGPSTime dat))::Double
    tinjdat = fromIntegral (fst (startGPSTime injdat))
         + 1E-9 * fromIntegral (snd (startGPSTime injdat))::Double
    timeSlide = floor $ (tinjdat - tdat)*(samplingFrequency dat)


addInjsig n v w = runSTVector $ do
  v' <- unsafeThawVector v
  mapM_ (\i -> addInjsigCore v' w (n+i) i) [0 .. nw-1]
  return v'
  where
    nw = dim w

addInjsigCore :: STVector s Double -> Vector Double -> Int -> Int -> ST s ()
addInjsigCore v w i j = modifyVector v i (+w@>j)





