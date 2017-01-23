{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module HasKAL.MonitorUtils.CorrelationMon.MIC
( micV
, micU
, micU'
, micWave
)
where


-- | General functions
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Unboxed as U
import System.IO.Unsafe (unsafePerformIO)
-- | for R functions
import qualified Language.R as R
import Language.R (R)
import Language.R.QQ
import qualified H.Prelude as H
-- | for HasKAL functions
import HasKAL.SignalProcessingUtils.Resampling
import HasKAL.WaveUtils.Data(WaveData(..))


type MIC = Double
type MAS = Double
type MEV = Double
type MCN = Double
type MIC_R2 = Double
type GMIC = Double


-- | micV v1 v2
micWave :: WaveData
        -> WaveData
        -> (MIC,MAS,MEV,MCN,MIC_R2,GMIC)
micWave w1 w2 = unsafePerformIO $ do
  let (v1,v2) = arrangeFsV w1 w2
  let (a,b) = arrangeDataV v1 v2
  input <- R.withEmbeddedR R.defaultConfig
    $ R.runRegion $ r_mic a b
  return $ toMICdata input
  where
    toMICdata v = (v!!0,v!!1,v!!2,v!!3,v!!4,v!!5)


arrangeFsV w1 w2 = do
  let fs1 = samplingFrequency w1
      fs2 = samplingFrequency w2
   in case () of
        _
          | fs1 == fs2 -> (gwdata w1, gwdata w2)
          | fs1 > fs2 -> (downsampleSV fs1 fs2 (gwdata w1), gwdata w2)
          | fs1 < fs2 -> (gwdata w2, downsampleSV fs2 fs1 (gwdata w2))


-- | micV v1 v2
micV :: V.Vector Double
     -> V.Vector Double
     -> (MIC,MAS,MEV,MCN,MIC_R2,GMIC)
micV v1 v2 = unsafePerformIO $ do
  let (a,b) = arrangeDataV v1 v2
  input <- R.withEmbeddedR R.defaultConfig
    $ R.runRegion $ r_mic a b
  return $ toMICdata input
  where
    toMICdata v = (v!!0,v!!1,v!!2,v!!3,v!!4,v!!5)


-- | micU v1 v2
micU :: U.Vector Double
     -> U.Vector Double
     -> (MIC,MAS,MEV,MCN,MIC_R2,GMIC)
micU v1 v2 = unsafePerformIO $ do
  let (a,b) = arrangeDataU v1 v2
  input <- R.withEmbeddedR R.defaultConfig
    $ R.runRegion $ r_mic a b
  return $ toMICdata input
  where
    toMICdata v = (v!!0,v!!1,v!!2,v!!3,v!!4,v!!5)


micU' :: U.Vector Double
      -> U.Vector Double
      -> Double
micU' v1 v2 = unsafePerformIO $ do
  let (a,b) = arrangeDataU v1 v2
  input <- R.withEmbeddedR R.defaultConfig
    $ R.runRegion $ r_mic a b
  return $ toMICdata input
  where
    toMICdata v = v!!0


-- | load mic from R
r_mic :: [Double] -> [Double] -> R s [Double]
r_mic x y = do
    H.dynSEXP <$> [r| require("minerva")
                      a <- mine(x_hs,y_hs)
                      c(a$MIC, a$MAS, a$MEV, a$MCN, a$`MIC-R2`,a$GMIC)
                      |]


-- | helper Function
arrangeDataV :: V.Vector Double
             -> V.Vector Double
             -> ([Double],[Double])
arrangeDataV v1 v2
  | V.length v1 == V.length v2 = (V.toList v1, V.toList v2)
  | V.length v1 > V.length v2 = (take (V.length v2) (V.toList v1), V.toList v2)
  | V.length v1 < V.length v2 = (V.toList v2,take (V.length v1) (V.toList v2))
  | otherwise = error "arrangeData: something wrong."


-- | helper Function
arrangeDataU :: U.Vector Double
             -> U.Vector Double
             -> ([Double],[Double])
arrangeDataU v1 v2
  | U.length v1 == U.length v2 = (U.toList v1, U.toList v2)
  | U.length v1 > U.length v2 = (take (U.length v2) (U.toList v1), U.toList v2)
  | U.length v1 < U.length v2 = (U.toList v2,take (U.length v1) (U.toList v2))
  | otherwise = error "arrangeData: something wrong."
