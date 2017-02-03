{-# LANGUAGE ForeignFunctionInterface #-}

module HasKAL.StatisticsUtils.Correlation.DistCor
( distcor
, distcorWave
)
where

import qualified Data.Vector.Storable as V
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr_, newForeignPtr)
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Ptr
import System.IO.Unsafe
-- | for HasKAL functions
import HasKAL.SignalProcessingUtils.Resampling
import HasKAL.WaveUtils.Data(WaveData(..))


-- | distcor v1 v2
distcorWave :: WaveData
            -> WaveData
            -> Double
distcorWave w1 w2 =
  let (v1,v2) = arrangeFsV w1 w2
      (a,b) = arrangeDataV v1 v2
   in distcor a b


arrangeFsV w1 w2 = do
  let fs1 = samplingFrequency w1
      fs2 = samplingFrequency w2
   in case () of
        _
          | fs1 == fs2 -> (gwdata w1, gwdata w2)
          | fs1 > fs2 -> (downsampleSV fs1 fs2 (gwdata w1), gwdata w2)
          | fs1 < fs2 -> (gwdata w2, downsampleSV fs2 fs1 (gwdata w2))



distcor :: V.Vector Double
        -> V.Vector Double
        -> Double
distcor v1' v2' = do
  let (v1,v2,len) = arrangeVLen v1' v2'
      cv1 = d2cdV v1
      cv2 = d2cdV v2
      clen = i2ci len
   in cd2d $ head $ distorC cv1 cv2 clen


distorC :: V.Vector CDouble
        -> V.Vector CDouble
        -> CInt
        -> [CDouble]
distorC cv1 cv2 clen
  = unsafePerformIO $ V.unsafeWith cv1 $ \ptrcv1 ->
      V.unsafeWith cv2 $ \ptrcv2 ->
        allocaArray 1 $ \ptrout ->
        do c'distcor ptrcv1 ptrcv2 clen ptrout
           peekArray 1 ptrout


arrangeVLen :: V.Vector Double
            -> V.Vector Double
            -> (V.Vector Double, V.Vector Double, Int )
arrangeVLen v1 v2
  | V.length v1 == V.length v2 = (v1,v2,V.length v1)
  | V.length v1 > V.length v2  = (V.drop (V.length v1-V.length v2) v1,v2,V.length v2)
  | V.length v2 > V.length v1  = (V.drop (V.length v2-V.length v1) v1,v2,V.length v1)
  | otherwise = error "somethig wrong"


-- | helper Function
arrangeDataV :: V.Vector Double
             -> V.Vector Double
             -> (V.Vector Double,V.Vector Double)
arrangeDataV v1 v2
  | V.length v1 == V.length v2 = (v1, v2)
  | V.length v1 > V.length v2 = (V.slice 0 (V.length v2) v1, v2)
  | V.length v1 < V.length v2 = (v1, V.slice 0 (V.length v1) v2)
  | otherwise = error "arrangeData: something wrong."

i2ci :: Int -> CInt
i2ci = fromIntegral

d2cd :: Double -> CDouble
d2cd = realToFrac

cd2d :: CDouble -> Double
cd2d = realToFrac

d2cdV :: V.Vector Double -> V.Vector CDouble
d2cdV = V.map realToFrac


foreign import ccall "distcor.h distcor" c'distcor :: Ptr CDouble
                                                   -> Ptr CDouble
                                                   -> CInt
                                                   -> Ptr CDouble
                                                   -> IO()
