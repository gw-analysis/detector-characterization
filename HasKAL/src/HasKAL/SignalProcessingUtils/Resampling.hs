

module HasKAL.SignalProcessingUtils.Resampling
( downsample
, downsampleV
, downsampleUV
, downsampleSV
, downsampleSV'
, upsample
, resample
, resampleSV
, downsampleWaveData
, downsampleWaveData'
, resampleWaveData
, downsampling
) where


import Control.Monad (when)
import Control.Monad.ST (ST)
import Data.Vector.Generic.Mutable (unsafeWrite,new)
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed as UV
import HasKAL.SignalProcessingUtils.Cascade
import HasKAL.SignalProcessingUtils.Chebyshev(chebyshev1)
import HasKAL.SignalProcessingUtils.Filter
import HasKAL.SignalProcessingUtils.FilterType
import HasKAL.SignalProcessingUtils.ButterWorth
import HasKAL.TimeUtils.Function
import HasKAL.WaveUtils.Data (WaveData(..),mkWaveData)
import Numeric.LinearAlgebra

import Foreign.ForeignPtr (ForeignPtr, newForeignPtr_)
import Foreign.Ptr
import Foreign.Marshal.Array
import System.IO.Unsafe
import Data.Word
import Foreign.C.Types


downsampling :: Int -> Int -> SV.Vector Double -> SV.Vector Double
downsampling fs newfs inputV = do
  let ilen = SV.length inputV
      sfactor = fs `div` newfs
      olen = ilen `div` sfactor
      inputV' = d2cdV inputV :: SV.Vector CDouble
   in cd2dV $ downsampleCore sfactor ilen inputV' olen


downsampleCore :: Int -> Int -> SV.Vector CDouble -> Int -> SV.Vector CDouble
downsampleCore sfactor ilen input olen
  = unsafePerformIO $ SV.unsafeWith input $ \ptrInput ->
   allocaArray olen $ \ptrOutput ->
   do c'downsample wsfactor wilen ptrInput wolen ptrOutput
      newForeignPtr_ ptrOutput >>= \foreignptrOutput ->
        return $ SV.unsafeFromForeignPtr0 foreignptrOutput olen
      where wsfactor = itow32 sfactor
            wilen = itow32 ilen
            wolen = itow32 olen


downsample :: Double -> Double -> [Double] -> [Double]
downsample fs newfs x = y
  where y = snd.unzip $ filter (\(n, _) -> n `mod` p==1) $ zip [1..] x'
        p = truncate (fs/newfs)
        x' = toList $ iir lpf $ fromList x
        lpf = butter 4 fs (newfs/2) Low


upsample :: Double -> Double -> [Double] -> [Double]
upsample fs newfs x = undefined


resample :: Double -> Double -> [Double] -> [Double]
resample fs newfs x = undefined


downsampleWaveData :: Double -> WaveData -> WaveData
downsampleWaveData newfs x = y
  where y = mkWaveData (detector x) (dataType x) newfs (startGPSTime x) stopT v
        v = downsampleSV (samplingFrequency x) newfs (gwdata x)
        stopT = formatGPS $ deformatGPS (startGPSTime x) + 1/newfs*fromIntegral (dim (gwdata x))


downsampleWaveData' :: Double -> WaveData -> WaveData
downsampleWaveData' newfs x = y
  where y = mkWaveData (detector x) (dataType x) newfs (startGPSTime x) stopT v
        v = downsampleSV' (samplingFrequency x) newfs (gwdata x)
        stopT = formatGPS $ deformatGPS (startGPSTime x) + 1/newfs*fromIntegral (dim (gwdata x))


resampleWaveData :: Double -> WaveData -> WaveData
resampleWaveData newfs x = y
  where y = mkWaveData (detector x) (dataType x) newfs (startGPSTime x) stopT v
        v = resampleSV (samplingFrequency x) newfs (gwdata x)
        stopT = formatGPS $ deformatGPS (startGPSTime x) + 1/newfs*fromIntegral (dim (gwdata x))


downsampleV :: Double -> Double -> Vector Double -> Vector Double
downsampleV fs newfs x = y
  where y = if (p>=1) 
             then fromList $ snd.unzip $ filter (\(n, _) -> n `mod` p == 1) $ zip [1..] $ toList x'
             else error "new sample rate should be <= original sample rate."
        p = truncate (fs/newfs)
        x' = filtfilt lpf x
        lpf = butter 4 fs newfs2 Low
        newfs2 = 2*fs*tan (pi*newfs/2/fs)


downsampleUV :: Double -> Double -> UV.Vector Double -> UV.Vector Double
downsampleUV fs newfs v = 
  if (fs/newfs<1) 
    then error "new sample rate should be <= original sample rate."
    else
      UV.create $ do 
        vs <- new nvs
        let v' =  UV.convert $ filtfilt lpf $ UV.convert v
            lpf = butter 4 fs newfs2 Low
            newfs2 = 2*fs*tan (pi*newfs/2/fs)
        loop v' vs 0 nvs
        return vs
        where 
          n = UV.length v
          p = truncate $ fs/newfs
          nvs = n `div` p
          loop v vs i j = when (i < j) $ do
            unsafeWrite vs i (v UV.!(i*p))
            loop v vs (i+1) j
  

downsampleSV :: Double -> Double -> SV.Vector Double -> SV.Vector Double
downsampleSV fs newfs v = 
  if (fs/newfs<1) 
    then error "new sample rate should be <= original sample rate."
    else 
      let v' = filtfilt lpf v
          --lpf = butter 4 fs newfs2 Low
          lpf = chebyshev1 6 1 fs newfs2 Low
--          newfs2 = 2*fs*tan (pi*newfs/2/fs)
          newfs2 = 2*fs*tan (pi*newfs/fs/2)/(2*pi)
       in downsampling (floor fs) (floor newfs) v'
       

downsampleSV' :: Double -> Double -> SV.Vector Double -> SV.Vector Double
downsampleSV' fs newfs v = 
  if (fs/newfs<1) 
    then error "new sample rate should be <= original sample rate."
    else
      let v' = sosfiltfilt cascade v
          initCond = map calcInitCond cascade
          cascade = tf2cascade lpf
          lpf = chebyshev1 6 0.4 fs newfs2 Low
          newfs2 = 2*fs*tan (pi*newfs/fs/2)/(2*pi)
       in downsampling (floor fs) (floor newfs) v'

resampleSV :: Double -> Double -> SV.Vector Double -> SV.Vector Double
resampleSV fs newfs v = 
  if (fs/newfs<1) 
    then error "new sample rate should be <= original sample rate."
    else
      SV.create $ do 
        vs <- new nvs
        loop v vs 0 nvs
        return vs
        where 
          n = SV.length v
          p = truncate $ fs/newfs
          nvs = n `div` p
          loop v vs i j = when (i < j) $ do
            unsafeWrite vs i (v SV.!(i*p))
            loop v vs (i+1) j
 


itow32 :: Int -> CUInt
itow32 = fromIntegral

d2cd :: [Double] -> [CDouble]
d2cd = map realToFrac

cd2d :: [CDouble] -> [Double]
cd2d = map realToFrac

d2cdV :: SV.Vector Double -> SV.Vector CDouble
d2cdV = SV.map realToFrac

cd2dV :: SV.Vector CDouble -> SV.Vector Double
cd2dV = SV.map realToFrac



foreign import ccall "filterFunctions.h downsample" c'downsample :: CUInt -> CUInt -> Ptr CDouble -> CUInt ->  Ptr CDouble -> IO()



