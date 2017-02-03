{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module HasKAL.StatisticsUtils.Correlation.DistCorR
( dcorV
, dcorU
, dcorU'
, dcorWave
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


-- | dcor v1 v2
dcorWave :: WaveData
         -> WaveData
         -> Double
dcorWave w1 w2 = unsafePerformIO $ do
  let (v1,v2) = arrangeFsV w1 w2
  let (a,b) = arrangeDataV v1 v2
  input <- R.withEmbeddedR R.defaultConfig
    $ R.runRegion $ r_dcor a b
  return $ input


arrangeFsV w1 w2 = do
  let fs1 = samplingFrequency w1
      fs2 = samplingFrequency w2
   in case () of
        _
          | fs1 == fs2 -> (gwdata w1, gwdata w2)
          | fs1 > fs2 -> (downsampleSV fs1 fs2 (gwdata w1), gwdata w2)
          | fs1 < fs2 -> (gwdata w2, downsampleSV fs2 fs1 (gwdata w2))


-- | dcorV v1 v2
dcorV :: V.Vector Double
      -> V.Vector Double
      -> Double
dcorV v1 v2 = unsafePerformIO $ do
  let (a,b) = arrangeDataV v1 v2
  input <- R.withEmbeddedR R.defaultConfig
    $ R.runRegion $ r_dcor a b
  return input


-- | dcorU v1 v2
dcorU :: U.Vector Double
      -> U.Vector Double
      -> Double
dcorU v1 v2 = unsafePerformIO $ do
  let (a,b) = arrangeDataU v1 v2
  input <- R.withEmbeddedR R.defaultConfig
    $ R.runRegion $ r_dcor a b
  return input


dcorU' :: U.Vector Double
       -> U.Vector Double
       -> Double
dcorU' v1 v2 = unsafePerformIO $ do
  let (a,b) = arrangeDataU v1 v2
  input <- R.withEmbeddedR R.defaultConfig
    $ R.runRegion $ r_dcor a b
  return input


-- | load dcor from R
r_dcor :: [Double] -> [Double] -> R s Double
r_dcor x y = do
    H.dynSEXP <$> [r| require("energy")
                      dcor(x_hs,y_hs, index=1.0)
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
