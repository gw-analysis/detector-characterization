module HasKAL.MonitorUtils.ChirpletMon.Function
( module HasKAL.MonitorUtils.ChirpletMon.Data
, chirplet
, chirpletWave
, chirpletTrainWave
, catChirpletGram
) where

import Data.List (unzip3)
import qualified Data.Vector.Storable as V
import HasKAL.ExternalUtils.KAGALI.KAGALIUtils (dKGLChirpletMain)
import HasKAL.Misc.Function (mkChunksW)
import HasKAL.MonitorUtils.ChirpletMon.Data
import HasKAL.TimeUtils.Function (deformatGPS, formatGPS)
import HasKAL.WaveUtils.Data
import System.IO.Unsafe (unsafePerformIO)


chirplet :: ChirpletParam
         -> Double
         -> Double
         -> V.Vector Double
         -> ChirpletGram
chirplet p fs t0 v' = unsafePerformIO $ do
  let n = ipath p
      nmax = alpha p
      vlen' = V.length v'
  vlen <- case checkPowerof2 (fromIntegral vlen') of
    False -> do print "Warning: Data is not 2^J. Data is trucated."
                return $ 2^truncate (logBase 2 (fromIntegral vlen'))
    True  -> return $ 2^truncate (logBase 2 (fromIntegral vlen'))
  let v = V.slice 0 vlen v'
      (f, c) = dKGLChirpletMain v fs nmax n
      t = [ t0 + tt/fs
          | tt<-[0..(fromIntegral (V.length v-1)::Double)]
          ]
  return ChirpletGram
       { time = t
       , frequency = V.toList f
       , cost = [c]
       }


chirpletWave :: ChirpletParam
             -> WaveData
             -> ChirpletGram
chirpletWave p w = unsafePerformIO $ do
  let n = ipath p
      nmax = alpha p
      fs = samplingFrequency w
      v' = gwdata w
      vlen' = V.length v'
  vlen <- case checkPowerof2 (fromIntegral vlen') of
    False -> do print "Warning: Data is not 2^J. Data is trucated."
                return $ 2^truncate (logBase 2 (fromIntegral vlen'))
    True  -> return $ 2^truncate (logBase 2 (fromIntegral vlen'))
  let v = V.slice 0 vlen v'
      (f, c) = dKGLChirpletMain v fs nmax n
      t = [ deformatGPS (startGPSTime w) + tt/fs
          | tt<-[0..(fromIntegral (V.length v-1)::Double)]
          ]
  return ChirpletGram
       { time = t
       , frequency = V.toList f
       , cost = [c]
       }


chirpletTrainWave :: ChirpletParam
                  -> Double
                  -> Double
                  -> WaveData
                  -> [ChirpletGram]
chirpletTrainWave p dt odt w = unsafePerformIO $ do
  let fs = samplingFrequency w
  nsegment <- case checkPowerof2 (dt * fs) of
    False -> do print "Warning: Data is not 2^J. Data is trucated."
                return $ 2^truncate (logBase 2 (dt * fs))
    True  -> return $ 2^truncate (logBase 2 (dt * fs))
  let noverlap = floor $ odt * fs
      ws = mkChunksW w noverlap nsegment
   in return $ flip map ws $ \x -> chirpletWave p x


catChirpletGram :: [ChirpletGram]
                -> ChirpletGram
catChirpletGram cps = unsafePerformIO $ do
  let (x,y,z) = unzip3 $ [(time cp, frequency cp, cost cp) | cp<-cps]
  return ChirpletGram
        { time = concat x
        , frequency = concat y
        , cost = concat z
        }


{- helper functions-}
checkPowerof2 :: Double -> Bool
checkPowerof2 x | logBase 2 x - fromIntegral (truncate (logBase 2 x)) == 0 = True
                | otherwise = False
