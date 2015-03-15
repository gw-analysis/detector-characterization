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

ndsGetData :: String -> Int -> (String,Double) -> Int -> Int -> Int -> Int -> [Float]
ndsGetData server port (channelName,channelRate) gpsStart gpsEnd delta len = do
	withCString server $ \c'server -> do
	  let c'port = fromIntegral port :: CInt
	  withCString channelName $ \c'channelName -> do
			let c'gpsStart = fromIntegral gpsStart :: CInt
			    c'gpsEnd   = fromIntegral gpsEnd :: CInt
		      c'delta    = fromIntegral delta :: CInt
					c'len      = fromIntegral len :: CInt
			    c'dat      = ndsGetData' c'server c'port c'channelList c'gpsStart c'gpsEnd c'delta c'len
			map realToFrac c'dat :: [Float]
					


ndsGetData' :: CString -> CInt -> (CString,Double) -> CInt -> CInt -> CInt -> CInt -> [CFloat]
ndsGetData' c'server c'port c'channelList c'gpsStart c'gpsEnd c'delta c'len
  = unsafePerformIO $ 
	  withAray c'channelNames $ \ptr'channelList -> do
			allocaArray len $ \ptr'dat -> do
		c'ndsGetData c'server c'port ptr'channelNames c'gpsStart c'gpsEnd c'delta ptr'dat c'len
      peekArray (fromIntegral c'len) ptr'dat
	  where 
			(c'channelNames,c'channelRates) = c'channelList
	    len = (c'gpsEnd' - c'gpsStart') * channelRates
			c'gpsEnd' = fromIntegral c'gpsEnd :: Double
			c'gpsStart' = fromIntegral c'gpsStart :: Double
			

foreign import ccall "ndsGetData.h ndsGetData" c'ndsGetData :: CString -> CInt -> Ptr CString -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> Ptr CInt -> IO CInt






