{-# LANGUAGE ForeignFunctionInterface #-}

module HasKAL.SignalProcessingUtils.Filter
  (
  iirFilter,
  firFilter
  ) where

import Foreign.C.Types
-- import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Array
import System.IO.Unsafe



iirFilter :: [Double] -> Int -> [Double] -> [Double] -> Int -> [Double]
iirFilter input ilen numCoeff denomCoeff flen = do
  let input' = d2cd input
      numCoeff' = d2cd numCoeff
      denomCoeff' = d2cd denomCoeff
  cd2d $ iirFilterCore input' ilen numCoeff' denomCoeff' flen

firFilter :: [Double] -> Int -> [Double] -> Int -> [Double]
firFilter input ilen firCoeff flen = do
  let input' = d2cd input
      firCoeff' = d2cd firCoeff
  cd2d $ firFilterCore input' ilen firCoeff' flen



-------------  Internal Functions  -----------------------------

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

firFilterCore :: [CDouble] -> Int -> [CDouble] -> Int -> [CDouble]
firFilterCore input ilen firCoeff flen
  = unsafePerformIO $ withArray input $ \ptrInput ->
   withArray firCoeff $ \ptrFirCoeff ->
   allocaArray ilen $ \ptrOutput ->
   do c_fir_filter ptrInput wilen ptrFirCoeff wflen ptrOutput
      peekArray ilen ptrOutput
      where wilen = itow32 ilen
            wflen = itow32 flen


itow32 :: Int -> CUInt
itow32 = fromIntegral

d2cd :: [Double] -> [CDouble]
d2cd = map realToFrac

cd2d :: [CDouble] -> [Double]
cd2d = map realToFrac


foreign import ccall "filterFunctions.h iir_filter" c_iir_filter :: Ptr CDouble -> CUInt ->  Ptr CDouble -> Ptr CDouble -> CUInt -> Ptr CDouble -> IO()

foreign import ccall "filterFunctions.h fir_filter" c_fir_filter :: Ptr CDouble -> CUInt ->  Ptr CDouble -> CUInt -> Ptr CDouble -> IO()


