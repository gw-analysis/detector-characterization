{-# LANGUAGE ForeignFunctionInterface #-}

module Function
    (ndsGetData
    ) where

import Data.List (foldl')
import Foreign.C.Types (CFloat,CInt)
import Foreign.C.String (CString,withCString)
import Foreign.Ptr (Ptr)
import System.IO.Unsafe (unsafePerformIO)
import Foreign.Marshal.Array (withArray, peekArray, allocaArray)
--import Foreign.Marshal.Utils (with)


ndsGetData :: String
           -> Int
           -> [(String,Double)]
           -> Int
           -> Int
           -> Int
           -> Int
           -> [Float]
ndsGetData server port channelList gpsStart gpsEnd delta len =
 unsafePerformIO $ do
	withCString server $ \c'server -> do
	  let c'port = fromIntegral port :: CInt
	  let c'channelList = map conv channelList
		let c'gpsStart = fromIntegral gpsStart :: CInt
		    c'gpsEnd   = fromIntegral gpsEnd :: CInt
		    c'delta    = fromIntegral delta :: CInt
				c'len      = fromIntegral len :: CInt
		    c'dat      = ndsGetData' c'server c'port c'channelList c'gpsStart c'gpsEnd c'delta c'len
		return $  map realToFrac c'dat :: [Float]


conv (s,d) = unsafePerformIO $ withCString c $ \cs -> do
  let cd = realToFrac d :: CDouble
  return (cs,cd)


ndsGetData' :: CString
            -> CInt
            -> [(CString,Double)]
            -> CInt
            -> CInt
            -> CInt
            -> CInt
            -> [CFloat]
ndsGetData' c'server c'port c'channelList c'gpsStart c'gpsEnd c'delta c'len
  = unsafePerformIO $
	  withAray c'channelNames $ \ptr'channelList -> do
			allocaArray len $ \ptr'dat -> do
		  c'ndsGetData c'server c'port ptr'channelNames c'gpsStart c'gpsEnd c'delta ptr'dat c'len
      peekArray (fromIntegral c'len) ptr'dat
	  where
			(c'channelNames,c'channelRates) = unzip c'channelList
	    len = foldl' (+) 0 $ map ((c'gpsEnd' - c'gpsStart') *) c'channelRates


foreign import ccall "ndsGetData.h ndsGetData" c'ndsGetData :: CString -> CInt -> Ptr CString -> CInt -> CInt -> CInt -> CInt -> Ptr CFloat -> Ptr CInt -> IO CInt
