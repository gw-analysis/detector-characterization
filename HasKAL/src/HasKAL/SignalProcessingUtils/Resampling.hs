{-# LANGUAGE ForeignFunctionInterface #-}

module HasKAL.SignalProcessingUtils.Resampling
( downsample
, downsampleUV
, downsampleSV
, upsample
, resample
, resampleSV
, downsampleWaveData
, resampleWaveData
, resampleonlyWaveData
, downsampling
) where


import Control.Monad (when)
import Control.Monad.ST (ST)
import Data.Vector.Generic.Mutable (unsafeWrite,new)
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed as UV
import HasKAL.SignalProcessingUtils.Cascade
import HasKAL.SignalProcessingUtils.Chebyshev(chebyshev1)
import HasKAL.SignalProcessingUtils.FilterX(filtfilt0,calcInitCond)
import HasKAL.SignalProcessingUtils.Filter(sosfiltfilt)
import HasKAL.SignalProcessingUtils.FilterType
import HasKAL.SignalProcessingUtils.ButterWorth
import HasKAL.TimeUtils.Function
import HasKAL.WaveUtils.Data (WaveData(..),mkWaveData)
import Numeric.LinearAlgebra

import Foreign.ForeignPtr (ForeignPtr, newForeignPtr_, withForeignPtr, mallocForeignPtrArray0, touchForeignPtr)
import Foreign.Ptr
import Foreign.Marshal.Array
import System.IO.Unsafe
import Data.Word
import Foreign.C.Types
import Control.DeepSeq (deepseq, NFData)


dim = SV.length


downsampling :: Int -> Int -> SV.Vector Double -> SV.Vector Double
downsampling fs newfs inputV = do
  let ilen = SV.length inputV
      sfactor = fs `div` newfs
      olen = ilen `div` sfactor + 1
      inputV' = d2cdV inputV :: SV.Vector CDouble
   in cd2dV $ downsampleCore sfactor ilen inputV' olen


downsampleCore :: Int -> Int -> SV.Vector CDouble -> Int -> SV.Vector CDouble
downsampleCore sfactor ilen input olen
  = unsafePerformIO $ do
   let (fptrInput, inputLen) = SV.unsafeToForeignPtr0 input
   withForeignPtr fptrInput $ \ptrInput ->
     mallocForeignPtrArray0 olen >>= \fptrOutput -> withForeignPtr fptrOutput $ \ptrOutput ->
      do c'downsample wsfactor wilen ptrInput wolen ptrOutput
         return $ SV.unsafeFromForeignPtr0 fptrOutput olen
         where wsfactor = itow32 sfactor
               wilen = itow32 ilen
               wolen = itow32 olen


upsampling :: Int -> Int -> SV.Vector Double -> SV.Vector Double
upsampling fs newfs inputV = do
  let ilen = SV.length inputV
      sfactor = newfs `div` fs
      olen = ilen * sfactor
      inputV' = d2cdV inputV :: SV.Vector CDouble
   in cd2dV $ upsampleCore sfactor ilen inputV' olen


upsampleCore :: Int -> Int -> SV.Vector CDouble -> Int -> SV.Vector CDouble
upsampleCore sfactor ilen input olen
  = unsafePerformIO $ do
   let (fptrInput, inputLen) = SV.unsafeToForeignPtr0 input
   withForeignPtr fptrInput $ \ptrInput ->
     mallocForeignPtrArray0 olen >>= \fptrOutput -> withForeignPtr fptrOutput $ \ptrOutput ->
      do c'upsample wsfactor wilen ptrInput wolen ptrOutput
         return $ SV.unsafeFromForeignPtr0 fptrOutput olen
         where wsfactor = itow32 sfactor
               wilen = itow32 ilen
               wolen = itow32 olen



downsample :: Double -> Double -> [Double] -> [Double]
downsample fs newfs x = y
  where y = toList $ downsampling (floor fs) (floor newfs) x'
        x' = filtfilt0 lpf $ fromList x
        lpf = chebyshev1 6 1 fs newfs2 Low
        newfs2 = 2*fs*tan (2*pi*newfs/fs/2)/(2*pi)


upsample :: Double -> Double -> [Double] -> [Double]
upsample fs newfs x = undefined


resample :: Double -> Double -> [Double] -> [Double]
resample fs newfs x = undefined


downsampleWaveData :: Double -> WaveData -> WaveData
downsampleWaveData newfs x = y
  where y = mkWaveData (detector x) (dataType x) newfs (startGPSTime x) stopT v
        v = downsampleSV (samplingFrequency x) newfs (gwdata x)
        stopT = formatGPS $ deformatGPS (startGPSTime x) + 1/newfs*fromIntegral (dim (gwdata x))


sosDownsampleWaveData :: Double -> WaveData -> WaveData
sosDownsampleWaveData newfs x = y
  where y = mkWaveData (detector x) (dataType x) newfs (startGPSTime x) stopT v
        v = sosDownsampleSV (samplingFrequency x) newfs (gwdata x)
        stopT = formatGPS $ deformatGPS (startGPSTime x) + 1/newfs*fromIntegral (dim (gwdata x))


resampleWaveData :: (Int,Int) -> WaveData -> WaveData
resampleWaveData (p,q) x = y
  where y = mkWaveData (detector x) (dataType x) newfs (startGPSTime x) stopT v
        newfs = (fromIntegral p/fromIntegral q) * (samplingFrequency x)
        v = resampleSV (p,q) (samplingFrequency x) (gwdata x)
        stopT = formatGPS $ deformatGPS (startGPSTime x) + 1/newfs*fromIntegral (dim (gwdata x))


resampleonlyWaveData :: Double -> WaveData -> WaveData
resampleonlyWaveData newfs x = y
  where y = mkWaveData (detector x) (dataType x) newfs (startGPSTime x) stopT v
        v = resampleonlySV (samplingFrequency x) newfs (gwdata x)
        stopT = formatGPS $ deformatGPS (startGPSTime x) + 1/newfs*fromIntegral (dim (gwdata x))

downsampleUV :: Double -> Double -> UV.Vector Double -> UV.Vector Double
downsampleUV fs newfs v =
  if (fs/newfs<1)
    then error "new sample rate should be <= original sample rate."
    else
      UV.create $ do
        vs <- new nvs
        let v' =  UV.convert $ filtfilt0 lpf $ UV.convert v
            lpf = chebyshev1 6 1 fs newfs2 Low
            newfs2 = 2*fs*tan (2*pi*newfs/2/fs)/(2*pi)
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
      let v' = filtfilt0 lpf v
          lpf = chebyshev1 6 1 fs newfs2 Low
          newfs2 = 2*fs*tan (2*pi*newfs/fs/2)/(2*pi)
       in downsampling (floor fs) (floor newfs) v'



sosDownsampleSV :: Double -> Double -> SV.Vector Double -> SV.Vector Double
sosDownsampleSV fs newfs v =
  if (fs/newfs<1)
    then error "new sample rate should be <= original sample rate."
    else
      let v' = sosfiltfilt cascade v
          initCond = map calcInitCond cascade
          cascade = tf2cascade lpf
          lpf = chebyshev1 6 0.4 fs newfs2 Low
          newfs2 = 2*fs*tan (2*pi*newfs/fs/2)/(2*pi)
       in downsampling (floor fs) (floor newfs) v'



upsampleSV :: Double -> Double -> SV.Vector Double -> SV.Vector Double
upsampleSV fs newfs v =
  if (fs/newfs>1)
    then error "new sample rate should be >= original sample rate."
    else
      let v' = upsampling (floor fs) (floor newfs) v
          lpf = chebyshev1 6 1 fs lfsa Low
          lfsa = 2*fs*tan (2*pi*fs/fs/2)/(2*pi)

       in filtfilt0 lpf v'


-- | fs -> p/q x fs
resampleSV :: (Int,Int) -> Double -> SV.Vector Double -> SV.Vector Double
resampleSV (p,q) fs v =
  let up |p==1 = v
         |otherwise = upsampling (floor fs) (floor (fs*(fromIntegral p))) v
      lfs |fromIntegral p/fromIntegral q >=1 = fs
          |fromIntegral p/fromIntegral q <1  = (fromIntegral p)/(fromIntegral q)*fs
      lfsa = 2*fs*tan (2*pi*lfs/fs/2)/(2*pi)
      lpf = chebyshev1 6 1 (fs*fromIntegral p) lfsa Low
      upL = filtfilt0 lpf up
   in case q of
        1 -> up
        _ -> downsampling (floor (fs*(fromIntegral p))) (floor (fs*(fromIntegral p)/(fromIntegral q))) upL



resampleonlySV :: Double -> Double -> SV.Vector Double -> SV.Vector Double
resampleonlySV fs newfs v =
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



foreign import ccall "resampling.h downsample" c'downsample :: CUInt -> CUInt -> Ptr CDouble -> CUInt ->  Ptr CDouble -> IO()

foreign import ccall "resampling.h upsample" c'upsample :: CUInt -> CUInt -> Ptr CDouble -> CUInt ->  Ptr CDouble -> IO()
