{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HasKAL.ExternalUtils.LIGO.NDS2.Function
( Daq_channel_t (..)
, DaqDataType (..)
, ChanType (..)
, getData
, getCurrentData
, getChannels
, getCurrentChannels
, getNumberOfChannels
, getCurrentNumberOfChannels
, getDataStdout
) where

import Data.List (foldl')
import qualified Data.Vector.Storable as V
import Data.Word (Word32)
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
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
  { ch_name :: String
  , ch_rate :: Double
  , ch_tpnum :: Int
  , ch_bps :: Int
  , ch_chNum :: Int
  , ch_signal_gain :: Double
  , ch_signal_slope :: Double
  , ch_signal_offset :: Double
  , ch_signal_units :: String
  } deriving (Show, Eq, Read)


convDaqCh :: (String, Double, Int, Int, Int, Double, Double, Double, String)
          -> Daq_channel_t
convDaqCh (a,b,c,d,e,f,g,h,i) = Daq_channel_t
  { ch_name = a
  , ch_rate = b
  , ch_tpnum = c
  , ch_bps = d
  , ch_chNum = e
  , ch_signal_gain = f
  , ch_signal_slope = g
  , ch_signal_offset = h
  , ch_signal_units = i
  }


c2h :: (CString, CDouble, CInt, CInt, CInt, CFloat, CFloat, CFloat, CString)
    -> (String, Double, Int, Int, Int, Double, Double, Double, String)
c2h (a,b,c,d,e,f,g,h,i) = ( unsafePerformIO $ peekCString a
                          , realToFrac b
                          , fromIntegral c
                          , fromIntegral d
                          , fromIntegral e
                          , realToFrac f
                          , realToFrac g
                          , realToFrac h
                          , unsafePerformIO $ peekCString i
                          )

data CDaq_channel_t = CDaq_channel_t
  { c'daq_name :: CString
  , c'daq_rate :: CDouble
  , c'daq_tpnum :: CInt
  , c'daq_type :: ChanType
  , c'daq_bps :: CInt
  , c'daq_chNum :: CInt
  , c'daq_data_type :: DaqDataType
  , c'daq_signal_conv :: CSignal_conv_t
  }

instance Storable CDaq_channel_t where
  sizeOf _ = (#size daq_channel_t)
  alignment = sizeOf
  poke ptr_dqchan (CDaq_channel_t val_name
                                  val_rate
                                  val_tpnum
                                  val_type
                                  val_bps
                                  val_chNum
                                  val_data_type
                                  val_signal_conv) = do
    (#poke daq_channel_t, name)      ptr_dqchan val_name
    (#poke daq_channel_t, rate)      ptr_dqchan val_rate
    (#poke daq_channel_t, tpnum)     ptr_dqchan val_tpnum
    (#poke daq_channel_t, type)      ptr_dqchan val_type
    (#poke daq_channel_t, bps)       ptr_dqchan val_bps
    (#poke daq_channel_t, chNum)     ptr_dqchan val_chNum
    (#poke daq_channel_t, data_type) ptr_dqchan val_data_type
    (#poke daq_channel_t, s)         ptr_dqchan val_signal_conv
  peek ptr_dqchan = do
    val_name        <- (#peek daq_channel_t, name)      ptr_dqchan
    val_rate        <- (#peek daq_channel_t, rate)      ptr_dqchan
    val_tpnum       <- (#peek daq_channel_t, tpnum)     ptr_dqchan
    val_type        <- (#peek daq_channel_t, type)      ptr_dqchan
    val_bps         <- (#peek daq_channel_t, bps)       ptr_dqchan
    val_chNum       <- (#peek daq_channel_t, chNum)     ptr_dqchan
    val_data_type   <- (#peek daq_channel_t, data_type) ptr_dqchan
    val_signal_conv <- (#peek daq_channel_t, s)         ptr_dqchan
    return $ CDaq_channel_t { c'daq_name        = val_name
                            , c'daq_rate        = val_rate
                            , c'daq_tpnum       = val_tpnum
                            , c'daq_type        = val_type
                            , c'daq_bps         = val_bps
                            , c'daq_chNum       = val_chNum
                            , c'daq_data_type   = val_data_type
                            , c'daq_signal_conv = val_signal_conv
                            }


data CSignal_conv_t = CSignal_conv_t
  { c'daq_signal_gain :: CFloat
  , c'daq_signal_slope :: CFloat
  , c'daq_signal_offset :: CFloat
  , c'daq_signal_units :: CString
  }

instance Storable CSignal_conv_t where
  sizeOf _ = (#size signal_conv_t)
  alignment = sizeOf
  poke ptr_sigconv (CSignal_conv_t val_signal_gain
                                   val_signal_slope
                                   val_signal_offset
                                   val_signal_units) = do
    (#poke signal_conv_t, signal_gain)   ptr_sigconv val_signal_gain
    (#poke signal_conv_t, signal_slope)  ptr_sigconv val_signal_slope
    (#poke signal_conv_t, signal_offset) ptr_sigconv val_signal_offset
    (#poke signal_conv_t, signal_units)  ptr_sigconv val_signal_units
  peek ptr_sigconv = do
    val_signal_gain   <- (#peek signal_conv_t, signal_gain)   ptr_sigconv
    val_signal_slope  <- (#peek signal_conv_t, signal_slope)  ptr_sigconv
    val_signal_offset <- (#peek signal_conv_t, signal_offset) ptr_sigconv
    val_signal_units  <- (#peek signal_conv_t, signal_units)  ptr_sigconv
    return $ CSignal_conv_t { c'daq_signal_gain   = val_signal_gain
                            , c'daq_signal_slope  = val_signal_slope
                            , c'daq_signal_offset = val_signal_offset
                            , c'daq_signal_units  = val_signal_units
                            }


getDataStdout :: String
              -> Int
              -> [(String,Double)]
              -> Int
              -> Int
              -> Int
              -> IO()
getDataStdout server port channelList gpsStart gpsEnd delta =
  withCString server $ \c'server -> do
    let c'port = fromIntegral port :: CInt
        c'channelList = map conv channelList
        c'nch = fromIntegral (length channelList) :: CInt
        c'gpsStart = fromIntegral gpsStart :: CInt
        c'gpsEnd   = fromIntegral gpsEnd :: CInt
        c'delta    = fromIntegral delta :: CInt
    getDataStdout' c'server c'port c'channelList c'nch c'gpsStart c'gpsEnd c'delta


getDataStdout' :: CString
               -> CInt
               -> [(CString,CDouble)]
               -> CInt
               -> CInt
               -> CInt
               -> CInt
               -> IO()
getDataStdout' c'server c'port c'channelList c'nch c'gpsStart c'gpsEnd c'delta =
  withArray c'channelNames $ \ptr'channelNames ->
    c'GetDataStdout c'server c'port ptr'channelNames c'nch c'gpsStart c'gpsEnd c'delta
   where
     (c'channelNames,c'channelRates) = unzip c'channelList


getCurrentData :: String
               -> Int
               -> [(String,Double)]
               -> Int
               -> Int
               -> [[V.Vector Double]]
getCurrentData server port channelList duration delta =
 unsafePerformIO $ do
   withCString server $ \c'server -> do
    let c'port = fromIntegral port :: CInt
        c'channelList = map conv channelList
        c'nch = fromIntegral (length channelList) :: CInt
        c'gpsStart = fromIntegral 0 :: CInt
        c'gpsEnd   = fromIntegral duration :: CInt
        c'delta    = fromIntegral delta :: CInt
        c'dat      = getData' c'server c'port c'channelList c'nch c'gpsStart c'gpsEnd c'delta
    return $ flip map c'dat $ \b-> (flip map b $ \a -> V.fromList (map realToFrac a))


getData :: String
        -> Int
        -> [(String,Double)]
        -> Int
        -> Int
        -> Int
        -> [[V.Vector Double]]
getData server port channelList gpsStart gpsEnd delta =
 unsafePerformIO $ do
   withCString server $ \c'server -> do
    let c'port = fromIntegral port :: CInt
        c'channelList = map conv channelList
        c'nch = fromIntegral (length channelList) :: CInt
        c'gpsStart = fromIntegral gpsStart :: CInt
        c'gpsEnd   = fromIntegral gpsEnd :: CInt
        c'delta    = fromIntegral delta :: CInt
        c'dat      = getData' c'server c'port c'channelList c'nch c'gpsStart c'gpsEnd c'delta
    return $ flip map c'dat $ \b-> (flip map b $ \a -> V.fromList (map realToFrac a))


conv (s,d) = unsafePerformIO $ withCString s $ \cs -> do
  let cd = realToFrac d :: CDouble
  return (cs,cd)


getData' :: CString
         -> CInt
         -> [(CString,CDouble)]
         -> CInt
         -> CInt
         -> CInt
         -> CInt
         -> [[[CFloat]]]
getData' c'server c'port c'channelList c'nch c'gpsStart c'gpsEnd c'delta =
  unsafePerformIO $
    withArray c'channelNames $ \ptr'channelNames -> do
      allocaArray len $ \ptr'dat -> do
        allocaArray 1 $ \ptr'len -> do
          c'GetData c'server c'port ptr'channelNames c'nch c'gpsStart c'gpsEnd c'delta ptr'dat ptr'len
          c'len <- peekArray 1 ptr'len
          x <- peekArray (fromIntegral (head c'len)) ptr'dat
          let xl = mkChunksLCF x len
          return $ flip map xl $ \y -> sepdata y llen
     where
      (c'channelNames,c'channelRates) = unzip c'channelList
      len = foldl' (+) 0 $ map (((c'gpsEnd' - c'gpsStart') *) . floor . realToFrac) c'channelRates
      llen = map (((c'gpsEnd' - c'gpsStart') *) . floor . realToFrac) c'channelRates
      c'gpsEnd' = fromIntegral c'gpsEnd :: Int
      c'gpsStart' = fromIntegral c'gpsStart :: Int

sepdata x (l:ls) = take l x : sepdata (drop l x) ls


getCurrentChannels :: String
                   -> Int
                   -> [Daq_channel_t]
getCurrentChannels server port =
  unsafePerformIO $ do
   withCString server $ \c'server -> do
    let c'port = fromIntegral port :: CInt
        c'gps = fromIntegral 0 :: CInt
        c'dat = getChannels' c'server c'port c'gps
    return $ map (convDaqCh . c2h) c'dat :: IO [Daq_channel_t]


getChannels :: String
            -> Int
            -> Int
            -> [Daq_channel_t]
getChannels server port gps =
 unsafePerformIO $ do
   withCString server $ \c'server -> do
    let c'port = fromIntegral port :: CInt
        c'gps = fromIntegral gps :: CInt
        c'dat = getChannels' c'server c'port c'gps
    return $ map (convDaqCh . c2h) c'dat :: IO [Daq_channel_t]


getChannels' :: CString
             -> CInt
             -> CInt
             -> [(CString, CDouble, CInt, CInt, CInt, CFloat, CFloat, CFloat, CString)]
getChannels' c'server c'port c'gps =
  unsafePerformIO $ do
    let nchan = fromIntegral $ getNumberOfChannels' c'server c'port c'gps :: Int
    allocaArray nchan $ \ptr'name -> do
      allocaArray nchan $ \ptr'rate -> do
        allocaArray nchan $ \ptr'tpnum -> do
          allocaArray nchan $ \ptr'bps -> do
            allocaArray nchan $ \ptr'chnum -> do
              allocaArray nchan $ \ptr'datatype -> do
                allocaArray nchan $ \ptr'gain -> do
                  allocaArray nchan $ \ptr'slope -> do
                    allocaArray nchan $ \ptr'offset -> do
                      allocaArray nchan $ \ptr'units -> do
                        c'GetChannels c'server
                                      c'port
                                      c'gps
                                      ptr'name
                                      ptr'rate
                                      ptr'tpnum
                                      ptr'bps
                                      ptr'chnum
                                      ptr'datatype
                                      ptr'gain
                                      ptr'slope
                                      ptr'offset
                                      ptr'units
                        name <- peekArray nchan ptr'name
                        rate <- peekArray nchan ptr'rate
                        tpnum <- peekArray nchan ptr'tpnum
                        bps <- peekArray nchan ptr'bps
                        chnum <- peekArray nchan ptr'chnum
                        gain <- peekArray nchan ptr'gain
                        slope <- peekArray nchan ptr'slope
                        offset <- peekArray nchan ptr'offset
                        units <- peekArray nchan ptr'units
                        return $ zip9 name rate tpnum bps chnum gain slope offset units


getCurrentNumberOfChannels :: String
                           -> Int
                           -> Int
getCurrentNumberOfChannels server port =
  unsafePerformIO $ do
    withCString server $ \c'server -> do
      let c'port = fromIntegral port :: CInt
          c'gps = fromIntegral 0 :: CInt
          c'nchan = getNumberOfChannels' c'server c'port c'gps
      return $  fromIntegral c'nchan :: IO Int


getNumberOfChannels :: String
                    -> Int
                    -> Int
                    -> Int
getNumberOfChannels server port gps =
  unsafePerformIO $ do
    withCString server $ \c'server -> do
      let c'port = fromIntegral port :: CInt
          c'gps = fromIntegral gps :: CInt
          c'nchan = getNumberOfChannels' c'server c'port c'gps
      return $  fromIntegral c'nchan :: IO Int


getNumberOfChannels' :: CString
                     -> CInt
                     -> CInt
                     -> CInt
getNumberOfChannels' c'server c'port c'gps =
  unsafePerformIO $
    alloca $ \ptr'nchan -> do
      c'GetNumberOfChannels c'server c'port c'gps ptr'nchan
      lnchan <- peek ptr'nchan
      return lnchan


mkChunksLCF :: [CFloat] -> Int -> [[CFloat]]
mkChunksLCF lIn n = mkChunksLCFCore lIn n (Prelude.length lIn `div` n)
  where
    mkChunksLCFCore _ _ 0 = []
    mkChunksLCFCore lIn n m
      = Prelude.take n lIn :  mkChunksLCFCore (Prelude.drop n lIn) n (m-1)


zip9 :: [a]->[b]->[c]->[d]->[e]->[f]->[g]->[h]->[i] -> [(a,b,c,d,e,f,g,h,i)]
zip9 (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs) (h:hs) (i:is)
                       = (a,b,c,d,e,f,g,h,i) : zip9 as bs cs ds es fs gs hs is
zip9 _ _ _ _ _ _ _ _ _ = []


foreign import ccall "nds-related.h nds_GetData" c'GetData
  :: CString
  -> CInt
  -> Ptr CString
  -> CInt
  -> CInt
  -> CInt
  -> CInt
  -> Ptr CFloat
  -> Ptr CInt
  -> IO ()

foreign import ccall "nds-related.h nds_GetChannels" c'GetChannels
  :: CString
  -> CInt
  -> CInt
  -> Ptr CString
  -> Ptr CDouble
  -> Ptr CInt
  -> Ptr CInt
  -> Ptr CInt
  -> Ptr DaqDataType
  -> Ptr CFloat
  -> Ptr CFloat
  -> Ptr CFloat
  -> Ptr CString
  -> IO ()

foreign import ccall "nds-related.h nds_GetNumberOfChannels" c'GetNumberOfChannels
  :: CString
  -> CInt
  -> CInt
  -> Ptr CInt
  -> IO ()

foreign import ccall "nds-related.h nds_GetData_stdout" c'GetDataStdout
  :: CString
  -> CInt
  -> Ptr CString
  -> CInt
  -> CInt
  -> CInt
  -> CInt
  -> IO ()
