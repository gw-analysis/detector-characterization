{-# LANGUAGE ForeignFunctionInterface #-}

module HasKAL.ExternalUtils.LIGO.NDS2.Function
    (ndsGetData
    ) where

import Data.List (foldl')
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Ptr
import System.IO.Unsafe (unsafePerformIO)
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
        c'channelList = map conv channelList
        c'gpsStart = fromIntegral gpsStart :: CInt
        c'gpsEnd   = fromIntegral gpsEnd :: CInt
        c'delta    = fromIntegral delta :: CInt
        c'len      = fromIntegral len :: CInt
        c'dat      = ndsGetData' c'server c'port c'channelList c'gpsStart c'gpsEnd c'delta c'len
    return $  map realToFrac c'dat :: IO [Float]


conv (s,d) = unsafePerformIO $ withCString s $ \cs -> do
  let cd = realToFrac d :: CDouble
  return (cs,cd)


ndsGetData' :: CString
            -> CInt
            -> [(CString,CDouble)]
            -> CInt
            -> CInt
            -> CInt
            -> CInt
            -> [CFloat]
ndsGetData' c'server c'port c'channelList c'gpsStart c'gpsEnd c'delta c'len
  = unsafePerformIO $
     withArray c'channelNames $ \ptr'channelNames -> do
      allocaArray len $ \ptr'dat -> do
        c'ndsGetData c'server c'port ptr'channelNames c'gpsStart c'gpsEnd c'delta c'len ptr'dat
        peekArray (fromIntegral c'len) ptr'dat
     where
      (c'channelNames,c'channelRates) = unzip c'channelList
      len = foldl' (+) 0 $ map (((c'gpsEnd' - c'gpsStart') *) . floor . realToFrac) c'channelRates
      c'gpsEnd' = fromIntegral c'gpsEnd :: Int
      c'gpsStart' = fromIntegral c'gpsStart :: Int


foreign import ccall "ndsGetData.h ndsGetData" c'ndsGetData :: CString 
                                                            -> CInt 
                                                            -> Ptr CString 
                                                            -> CInt 
                                                            -> CInt 
                                                            -> CInt 
                                                            -> CInt 
                                                            -> Ptr CFloat 
                                                            -> IO ()
