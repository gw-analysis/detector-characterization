{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HasKAL.ExternalUtils.LIGO.NDS2.Function
( ndsGetData
, ndsGetChannels
, ndsGetNumberOfChannels
) where

import Data.List (foldl')
import Data.Word (Word32)
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe (unsafePerformIO)
--import Foreign.Marshal.Utils (with)

-- import include file of C
#include "channel.h"
#include "daqc.h"

newtype ChanType = ChanType #{type chantype_t}
  deriving (Storable, Show, Eq, Read)

#{enum ChanType, ChanType
 , hs_cUnknown = cUnknown
 , hs_cOnline = cOnline
 , hs_cRaw = cRaw
 , hs_cRDS = cRDS
 , hs_cSTrend = cSTrend
 , hs_cMTrend = cMTrend
 , hs_cTestPoint = cTestPoint
 , hs_cStatic = cStatic
}

newtype DaqDataType = DaqDataType #{type daq_data_t}
  deriving (Storable, Show, Eq, Read)

#{enum DaqDataType, DaqDataType
 , hs_undefined = _undefined
 , hs_16bit_integer = _16bit_integer
 , hs_32bit_integer = _32bit_integer
 , hs_64bit_integer = _64bit_integer
 , hs_32bit_float = _32bit_float
 , hs_64bit_double = _64bit_double
 , hs_32bit_complex = _32bit_complex
}

data Daq_channel_t = Daq_channel_t
  { daq_name :: CString
  , daq_rate :: CDouble
  , daq_tpnum :: CInt
  , daq_type :: ChanType
  , daq_bps :: CInt
  , daq_chNum :: CInt
  , daq_data_type :: DaqDataType
  , daq_signal_conv :: Signal_conv_t
  }

instance Storable Daq_channel_t where
  sizeOf = const #size struct daq_channel_
  alignment = sizeOf
  poke ptr_dqchan (Daq_channel_t val_name val_rate val_tpnum val_type val_bps val_chNum val_data_type val_signal_conv) = do
    (#poke struct daq_channel_, name) ptr_dqchan val_name
    (#poke struct daq_channel_, rate) ptr_dqchan val_rate
    (#poke struct daq_channel_, tpnum) ptr_dqchan val_tpnum
    (#poke struct daq_channel_, type) ptr_dqchan val_type
    (#poke struct daq_channel_, bps) ptr_dqchan val_bps
    (#poke struct daq_channel_, chNum) ptr_dqchan val_chNum
    (#poke struct daq_channel_, data_type) ptr_dqchan val_data_type
    (#poke struct daq_channel_, s) ptr_dqchan val_signal_conv
  peek ptr_dqchan = do
    val_name        <- (#peek struct daq_channel_, name) ptr_dqchan
    val_rate        <- (#peek struct daq_channel_, rate) ptr_dqchan
    val_tpnum       <- (#peek struct daq_channel_, tpnum) ptr_dqchan
    val_type        <- (#peek struct daq_channel_, type) ptr_dqchan
    val_bps         <- (#peek struct daq_channel_, bps) ptr_dqchan
    val_chNum       <- (#peek struct daq_channel_, chNum) ptr_dqchan
    val_data_type   <- (#peek struct daq_channel_, data_type) ptr_dqchan
    ptr_signal_conv <- (#peek struct daq_channel_, s) ptr_dqchan
    return $ Daq_channel_t { daq_name = val_name
                           , daq_rate = val_rate
                           , daq_tpnum = val_tpnum
                           , daq_type = val_type
                           , daq_bps = val_bps
                           , daq_chNum = val_chNum
                           , daq_data_type = val_data_type
                           , daq_signal_conv = ptr_signal_conv
                           }

data Signal_conv_t = Signal_conv_t
  { daq_signal_gain :: CFloat
  , daq_signal_slope :: CFloat
  , daq_signal_offset :: CFloat
  , daq_signal_units :: CString
}

instance Storable Signal_conv_t where
  sizeOf = const #size struct signal_conv_
  alignment = sizeOf
  poke ptr_sigconv (Signal_conv_t val_signal_gain val_signal_slope val_signal_offset val_signal_units) = do
    (#poke struct signal_conv_, signal_gain) ptr_sigconv val_signal_gain
    (#poke struct signal_conv_, signal_slope) ptr_sigconv val_signal_slope
    (#poke struct signal_conv_, signal_offset) ptr_sigconv val_signal_offset
    (#poke struct signal_conv_, signal_units) ptr_sigconv val_signal_units
  peek ptr_sigconv = do
    val_signal_gain   <- (#peek struct signal_conv_, signal_gain) ptr_sigconv
    val_signal_slope  <- (#peek struct signal_conv_, signal_slope) ptr_sigconv
    val_signal_offset <- (#peek struct signal_conv_, signal_offset) ptr_sigconv
    val_signal_units  <- (#peek struct signal_conv_, signal_units) ptr_sigconv
    return $ Signal_conv_t { daq_signal_gain = val_signal_gain
                           , daq_signal_slope = val_signal_slope
                           , daq_signal_offset = val_signal_offset
                           , daq_signal_units = val_signal_units
                           }


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
ndsGetData' c'server c'port c'channelList c'gpsStart c'gpsEnd c'delta c'len =
  unsafePerformIO $
    withArray c'channelNames $ \ptr'channelNames -> do
      allocaArray len $ \ptr'dat -> do
        c'ndsGetData c'server c'port ptr'channelNames c'gpsStart c'gpsEnd c'delta c'len ptr'dat
        peekArray (fromIntegral c'len) ptr'dat
     where
      (c'channelNames,c'channelRates) = unzip c'channelList
      len = foldl' (+) 0 $ map (((c'gpsEnd' - c'gpsStart') *) . floor . realToFrac) c'channelRates
      c'gpsEnd' = fromIntegral c'gpsEnd :: Int
      c'gpsStart' = fromIntegral c'gpsStart :: Int


ndsGetChannels :: String
               -> Int
               -> Int
               -> [Daq_channel_t]
ndsGetChannels server port gps =
 unsafePerformIO $ do
   withCString server $ \c'server -> do
    let c'port = fromIntegral port :: CInt
        c'gps = fromIntegral gps :: CInt
        c'dat = ndsGetChannels' c'server c'port c'gps
    return $  c'dat :: IO [Daq_channel_t]


ndsGetChannels' :: CString
                -> CInt
                -> CInt
                -> [Daq_channel_t]
ndsGetChannels' c'server c'port c'gps =
  unsafePerformIO $ do
    let nchan = fromIntegral $ ndsGetNumberOfChannels' c'server c'port c'gps :: Int
    allocaArray nchan $ \ptr'nchan -> do
      allocaArray nchan $ \ptr'dqchant -> do
        c'ndsGetChannels c'server c'port c'gps ptr'nchan ptr'dqchant
        peekArray nchan ptr'dqchant


ndsGetNumberOfChannels :: String
                       -> Int
                       -> Int
                       -> Int
ndsGetNumberOfChannels server port gps =
  unsafePerformIO $ do
    withCString server $ \c'server -> do
      let c'port = fromIntegral port :: CInt
          c'gps = fromIntegral gps :: CInt
          c'nchan = ndsGetNumberOfChannels' c'server c'port c'gps
      return $  fromIntegral c'nchan :: IO Int


ndsGetNumberOfChannels' :: CString
                        -> CInt
                        -> CInt
                        -> CInt
ndsGetNumberOfChannels' c'server c'port c'gps =
  unsafePerformIO $
    allocaArray 1 $ \ptr'nchan -> do
      c'ndsGetNumberOfChannels c'server c'port c'gps ptr'nchan
      lnchan <- peekArray 1 ptr'nchan
      return $ head lnchan


foreign import ccall "nds-related.h nds_GetData" c'ndsGetData :: CString
                                                              -> CInt
                                                              -> Ptr CString
                                                              -> CInt
                                                              -> CInt
                                                              -> CInt
                                                              -> CInt
                                                              -> Ptr CFloat
                                                              -> IO ()

foreign import ccall "nds-related.h nds_GetChannels" c'ndsGetChannels :: CString
                                                                      -> CInt
                                                                      -> CInt
                                                                      -> Ptr CInt
                                                                      -> Ptr Daq_channel_t
                                                                      -> IO ()

foreign import ccall "nds-related.h nds_GetNumberOfChannels" c'ndsGetNumberOfChannels :: CString
                                                                                      -> CInt
                                                                                      -> CInt
                                                                                      -> Ptr CInt
                                                                                      -> IO ()
