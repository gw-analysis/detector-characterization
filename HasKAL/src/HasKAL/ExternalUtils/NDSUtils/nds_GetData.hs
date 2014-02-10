{-# LANGUAGE ForeignFunctionInterface #-}

module NDSUtils
    (c_nds_GetData
    ) where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr

foreign import ccall "nds_functions.h nds_GetData" c_nds_GetData :: CString -> CInt -> Ptr CString -> CInt -> CInt -> CInt -> CInt




