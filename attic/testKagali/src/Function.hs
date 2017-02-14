module Function
( ChirpletParam
, ChirpletGram
, chirplet
, chirpletWave
, chirpletTrainWave
, catChirpletGram
) where

import Data.List (unzip3)
import qualified Data.Vector.Storable as V
import HasKAL.ExternalUtils.KAGALI.KAGALIUtils (dKGLChirpletMain)
import HasKAL.Misc.Function (mkChunksW)
import HasKAL.TimeUtils.Function (deformatGPS, formatGPS)
import HasKAL.WaveUtils.Data


data ChirpletParam = ChirpletParam
  { alpha :: Double
  , ipath :: Int
  } deriving (Show, Eq, Read)

data ChirpletGram = ChirpletGram
  { time      :: [Double]
  , frequency :: [Double]
  , cost      :: [Double]
  } deriving (Show, Eq, Read)


chirplet :: ChirpletParam
         -> Double
         -> Double
         -> V.Vector Double
         -> ChirpletGram
chirplet p fs t0 v =
  let n = ipath p
      nmax = alpha p
      (f, c) = dKGLChirpletMain v fs nmax n
      t = [ t0 + tt/fs
          | tt<-[0..(fromIntegral (V.length v-1)::Double)]
          ]
   in ChirpletGram
       { time = t
       , frequency = V.toList f
       , cost = [c]
       }


chirpletWave :: ChirpletParam
             -> WaveData
             -> ChirpletGram
chirpletWave p w = do
  let n = ipath p
      nmax = alpha p
      fs = samplingFrequency w
      v = gwdata w
      (f, c) = dKGLChirpletMain v fs nmax n
      t = [ deformatGPS (startGPSTime w) + tt/fs
          | tt<-[0..(fromIntegral (V.length v-1)::Double)]
          ]
   in ChirpletGram
       { time = t
       , frequency = V.toList f
       , cost = [c]
       }


chirpletTrainWave :: ChirpletParam
                  -> Double
                  -> Double
                  -> WaveData
                  -> [ChirpletGram]
chirpletTrainWave p dt odt w = do
  let fs = samplingFrequency w
      nsegment = floor $ dt * fs
      noverlap = floor $ odt * fs
      ws = mkChunksW w noverlap nsegment
   in flip map ws $ \x -> chirpletWave p x


catChirpletGram :: [ChirpletGram]
                -> ChirpletGram
catChirpletGram cps = do
  let (x,y,z) = unzip3 $ [(time cp, frequency cp, cost cp) | cp<-cps]
   in ChirpletGram
        { time = concat x
        , frequency = concat y
        , cost = concat z
        }
