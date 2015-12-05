
{-# LANGUAGE ForeignFunctionInterface #-}

module HasKAL.SignalProcessingUtils.Filter
  ( iir
  , fir
--  , fir'
  , iirFilter
  , firFilter
  , filtfilt
  , sosfilter
  , sos1filter
  , sosstatespace
  ) where

import qualified Data.Vector.Storable as VS (Vector, length, unsafeWith, unsafeFromForeignPtr0,map)
import Foreign.C.Types
-- import Foreign.C.String
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr_)
import Foreign.Ptr
import Foreign.Marshal.Array
import System.IO.Unsafe
import Unsafe.Coerce (unsafeCoerce)
import Data.Word


iir :: ([Double],[Double]) -> VS.Vector Double -> VS.Vector Double
iir (numCoeff, denomCoeff) inputV = do
  let ilen = VS.length inputV
      flen = length numCoeff
      inputV' = d2cdV inputV :: VS.Vector CDouble
      numCoeff' = d2cd numCoeff
      denomCoeff' = d2cd denomCoeff
  cd2dV $ iirCore inputV' ilen numCoeff' denomCoeff' flen


iirFilter :: [Double] -> [Double] -> [Double] -> [Double]
iirFilter input numCoeff denomCoeff = do
  let ilen = length input :: Int
      flen = length numCoeff
      input' = d2cd input
      numCoeff' = d2cd numCoeff
      denomCoeff' = d2cd denomCoeff
  cd2d $ iirFilterCore input' ilen numCoeff' denomCoeff' flen


fir :: [Double] -> VS.Vector Double -> VS.Vector Double
fir firCoeff inputV = do
  let ilen = VS.length inputV
      flen = length firCoeff
      inputV' = d2cdV inputV :: VS.Vector CDouble
      firCoeff' = d2cd firCoeff
  cd2dV $ firCore inputV' ilen firCoeff' flen


fir' :: [Double] -> VS.Vector Double -> VS.Vector Double
fir' firCoeff inputV = do
  let ilen = VS.length inputV
      flen = length firCoeff
--      inputV' = d2cdV inputV :: VS.Vector CDouble
--      firCoeff' = d2cd firCoeff
--  cd2dV $ firCore inputV' ilen firCoeff' flen
  firCore' inputV ilen firCoeff flen


firFilter :: [Double] -> [Double] -> [Double]
firFilter input firCoeff = do
  let ilen = length input :: Int
      flen = length firCoeff
      input' = d2cd input
      firCoeff' = d2cd firCoeff
  cd2d $ firFilterCore input' ilen firCoeff' flen


filtfilt :: ([Double],[Double]) -> VS.Vector Double -> VS.Vector Double
filtfilt (numCoeff, denomCoeff) inputV = do
  let ilen = VS.length inputV
      flen = length numCoeff
      inputV' = d2cdV inputV :: VS.Vector CDouble
      numCoeff' = d2cd numCoeff
      denomCoeff' = d2cd denomCoeff
  cd2dV $ filtfiltCore inputV' ilen numCoeff' denomCoeff' flen


sosfilter :: [([Double], [Double])] -> VS.Vector Double -> VS.Vector Double
sosfilter coeffs inputV =
  let (num, denom) = unzip coeffs
      nums = map (\i->d2cd $ map (!!i) num) [0..length (head num)-1]
      denoms = map (\i->d2cd $ map (!!i) denom) [0..length (head denom)-1]
      ilen = VS.length inputV
      nsec = length coeffs
      inputV' = d2cdV inputV :: VS.Vector CDouble
   in cd2dV $ sosfilterCore inputV' ilen (nums!!0) (nums!!1) (nums!!2) (denoms!!0) (denoms!!1) (denoms!!2) nsec


sos1filter :: ([Double], [Double]) -> VS.Vector Double -> VS.Vector Double
sos1filter (num, denom) inputV =
  let inputV' = d2cdV inputV :: VS.Vector CDouble
      ilen = VS.length inputV
      num' = d2cd num
      denom' = d2cd denom
      nsec = 1
   in cd2dV $ sosfilterCore inputV' ilen [num'!!0] [num'!!1] [num'!!2] [denom'!!0] [denom'!!1] [denom'!!2] nsec


-------------  Internal Functions  -----------------------------

cumsum :: [Double] -> [Double]
cumsum = scanl1 (+)


iirCore :: VS.Vector CDouble -> Int -> [CDouble] -> [CDouble] -> Int -> VS.Vector CDouble
iirCore input ilen numCoeff denomCoeff flen
  = unsafePerformIO $ VS.unsafeWith input $ \ptrInput ->
   withArray numCoeff $ \ptrNumCoeff ->
   withArray denomCoeff $ \ptrDenomCoeff ->
   allocaArray ilen $ \ptrOutput ->
   do c_iir_filter ptrInput wilen ptrNumCoeff ptrDenomCoeff wflen ptrOutput
      newForeignPtr_ ptrOutput >>= \foreignptrOutput ->
        return $ VS.unsafeFromForeignPtr0 foreignptrOutput ilen
      where wilen = itow32 ilen
            wflen = itow32 flen


firCore :: VS.Vector CDouble -> Int -> [CDouble] -> Int -> VS.Vector CDouble
firCore input ilen firCoeff flen
  = unsafePerformIO $ VS.unsafeWith input $ \ptrInput ->
   withArray firCoeff $ \ptrFirCoeff ->
   allocaArray ilen $ \ptrOutput ->
   do c_fir_filter ptrInput wilen ptrFirCoeff wflen ptrOutput
      newForeignPtr_ ptrOutput >>= \foreignptrOutput ->
        return $ VS.unsafeFromForeignPtr0 foreignptrOutput ilen
      where wilen = itow32 ilen
            wflen = itow32 flen


firCore' :: VS.Vector Double -> Int -> [Double] -> Int -> VS.Vector Double
firCore' input ilen firCoeff flen
  = unsafePerformIO $ VS.unsafeWith input $ \ptrInput ->
   withArray firCoeff $ \ptrFirCoeff ->
   allocaArray ilen $ \ptrOutput ->
   do c_fir_filter (castPtr ptrInput) wilen (castPtr ptrFirCoeff) wflen ptrOutput
      newForeignPtr_ (castPtr ptrOutput) >>= \foreignptrOutput ->
        return $ VS.unsafeFromForeignPtr0 foreignptrOutput ilen
      where wilen = itow32 ilen
            wflen = itow32 flen


iirFilterCore :: [CDouble] -> Int -> [CDouble] -> [CDouble] -> Int -> [CDouble]
iirFilterCore input ilen numCoeff denomCoeff flen
  = unsafePerformIO $ withArray input $ \ptrInput ->
   withArray numCoeff $ \ptrNumCoeff ->
   withArray denomCoeff $ \ptrDenomCoeff ->
   allocaArray ilen $ \ptrOutput ->
   do c_iir_filter ptrInput wilen ptrNumCoeff ptrDenomCoeff wflen ptrOutput
      peekArray ilen ptrOutput
      where wilen = itow32 ilen
            wflen = itow32 flen


iirFilterCoreInit :: [CDouble] -> Int -> [CDouble] -> [CDouble] -> Int -> [CDouble] -> [CDouble]
iirFilterCoreInit input ilen numCoeff denomCoeff flen initCoeff
  = unsafePerformIO $ withArray input $ \ptrInput ->
   withArray numCoeff $ \ptrNumCoeff ->
   withArray denomCoeff $ \ptrDenomCoeff ->
   withArray initCoeff $ \ptrInitCoeff ->
   allocaArray ilen $ \ptrOutput ->
   do c_iir_filter_core ptrInput wilen ptrNumCoeff ptrDenomCoeff wflen ptrInitCoeff ptrOutput
      peekArray ilen ptrOutput
      where wilen = itow32 ilen
            wflen = itow32 flen


firFilterCore :: [CDouble] -> Int -> [CDouble] -> Int -> [CDouble]
firFilterCore input ilen firCoeff flen
  = unsafePerformIO $ withArray input $ \ptrInput ->
   withArray firCoeff $ \ptrFirCoeff ->
   allocaArray ilen $ \ptrOutput ->
   do c_fir_filter ptrInput wilen ptrFirCoeff wflen ptrOutput
      peekArray ilen ptrOutput
      where wilen = itow32 ilen
            wflen = itow32 flen



filtfiltCore :: VS.Vector CDouble -> Int -> [CDouble] -> [CDouble] -> Int -> VS.Vector CDouble
filtfiltCore input ilen numCoeff denomCoeff flen
  = unsafePerformIO $ VS.unsafeWith input $ \ptrInput ->
   withArray numCoeff $ \ptrNumCoeff ->
   withArray denomCoeff $ \ptrDenomCoeff ->
   allocaArray ilen $ \ptrOutput ->
   do c_filtfilt ptrInput wilen ptrNumCoeff ptrDenomCoeff wflen ptrOutput
      newForeignPtr_ ptrOutput >>= \foreignptrOutput ->
        return $ VS.unsafeFromForeignPtr0 foreignptrOutput ilen
      where wilen = itow32 ilen
            wflen = itow32 flen


sosfilterCore :: VS.Vector CDouble -> Int -> [CDouble] -> [CDouble] -> [CDouble] -> [CDouble] -> [CDouble] -> [CDouble] -> Int -> VS.Vector CDouble
sosfilterCore input ilen num0 num1 num2 denom0 denom1 denom2 nsec
  = unsafePerformIO $ VS.unsafeWith input $ \ptrInput ->
   withArray num0 $ \ptrNum0 ->
   withArray num1 $ \ptrNum1 ->
   withArray num2 $ \ptrNum2 ->
   withArray denom0 $ \ptrDenom0 ->
   withArray denom1 $ \ptrDenom1 ->
   withArray denom2 $ \ptrDenom2 ->
   allocaArray ilen $ \ptrOutput ->
   do c'sosfilter ptrInput wilen ptrNum0 ptrNum1 ptrNum2 ptrDenom0 ptrDenom1 ptrDenom2 wnsec ptrOutput
      newForeignPtr_ ptrOutput >>= \foreignptrOutput ->
        return $ VS.unsafeFromForeignPtr0 foreignptrOutput ilen
      where wilen = itow32 ilen
            wnsec = itow32 nsec


sosstatespace :: VS.Vector CDouble -> Int -> [CDouble] -> [CDouble] -> [CDouble] -> CDouble -> CDouble -> CDouble -> VS.Vector CDouble
sosstatespace input ilen a b c d x01 x02 
  = unsafePerformIO $ VS.unsafeWith input $ \ptrInput ->
   withArray a $ \ptrA ->
   withArray b $ \ptrB ->
   withArray c $ \ptrC ->
   allocaArray ilen $ \ptrOutput ->
   do c'sosstatespace ptrInput wilen ptrA ptrB ptrC d x01 x02 ptrOutput
      newForeignPtr_ ptrOutput >>= \foreignptrOutput ->
        return $ VS.unsafeFromForeignPtr0 foreignptrOutput ilen
      where wilen = itow32 ilen


itow32 :: Int -> CUInt
itow32 = fromIntegral

d2cd :: [Double] -> [CDouble]
d2cd = map realToFrac

cd2d :: [CDouble] -> [Double]
cd2d = map realToFrac

d2cdV :: VS.Vector Double -> VS.Vector CDouble
d2cdV = VS.map realToFrac

cd2dV :: VS.Vector CDouble -> VS.Vector Double
cd2dV = VS.map realToFrac

foreign import ccall "filterFunctions.h iir_filter" c_iir_filter :: Ptr CDouble -> CUInt ->  Ptr CDouble -> Ptr CDouble -> CUInt -> Ptr CDouble -> IO()

foreign import ccall "filterFunctions.h iir_filter_core" c_iir_filter_core :: Ptr CDouble -> CUInt ->  Ptr CDouble -> Ptr CDouble -> CUInt -> Ptr CDouble -> Ptr CDouble -> IO()

foreign import ccall "filterFunctions.h fir_filter" c_fir_filter :: Ptr CDouble -> CUInt ->  Ptr CDouble -> CUInt -> Ptr CDouble -> IO()

foreign import ccall "filterFunctions.h filtfilt" c_filtfilt :: Ptr CDouble -> CUInt ->  Ptr CDouble -> Ptr CDouble -> CUInt -> Ptr CDouble -> IO()

foreign import ccall "filterFunctions.h sosfilter" c'sosfilter :: Ptr CDouble -> CUInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CUInt -> Ptr CDouble -> IO()

foreign import ccall "filterFunction.h sosstatespace" c'sosstatespace :: Ptr CDouble -> CUInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CDouble -> CDouble -> CDouble -> Ptr CDouble -> IO ()
