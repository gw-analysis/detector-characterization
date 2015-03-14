{-# LANGUAGE ForeignFunctionInterface #-}

module NDSUtils
    (ndsGetData
    ) where

import Foreign.C.Types (CFloat,CInt)
import Foreign.C.String (CString)
import Foreign.Ptr (Ptr)
import System.IO.Unsafe (unsafePerformIO)
import Foreign.Marshal.Array (withArray, peekArray, allocaArray)
--import Foreign.Marshal.Utils (with)
ndsGetData' :: CString -> CInt -> (CString,Double) -> CInt -> CInt -> CInt -> CInt -> [CFloat]
ndsGetData' c'server c'port c'channellList c'gpsStart c'gpsEnd c'delta c'dat c'len
  = unsafePerformIO $ 
	  withAray c'channelNames $ \ptr'channelList -> do
			allocaArray len $ \ptr'dat -> do
		c'ndsGetData c'server c'port ptr'channelNames c'gpsStart c'gpsEnd c'delta ptr'dat c'len
      peekArray (fromIntegral c'len) ptr'dat
	  where 
			(c'channelNames,c'channelRates) = c'channellList
	    len = (c'gpsEnd' - c'gpsStart') * channelRates
			c'gpsEnd' = fromIntegral c'gpsEnd :: Double
			c'gpsStart' = fromIntegral c'gpsStart :: Double
			

foreign import ccall "ndsGetData.h ndsGetData" c'ndsGetData :: CString -> CInt -> Ptr CString -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> Ptr CInt -> IO CInt






