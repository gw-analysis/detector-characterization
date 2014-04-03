{-# LANGUAGE ForeignFunctionInterface #-}

module FilterUtils
  (
  c_iir_filter,
  c_fir_filter
  ) where

import Foreign.C.Types
-- import Foreign.C.String
import Foreign.Ptr

foreign import ccall "FilterUtiles.h c_iir_filter" c_iir_filter :: Ptr CDouble -> CUInt ->  Ptr CDouble -> Ptr CDouble -> CUInt -> Ptr CDouble

foreign import ccall " FilterUtiles.h c_fir_filter" c_fir_filter :: Ptr CDouble -> CUInt ->  Ptr CDouble -> CUInt -> Ptr CDouble
