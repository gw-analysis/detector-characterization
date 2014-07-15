----------------------------------------------------------------------
-- Modules for handling Frame formated file
--
-- frame library :
-- http://lappweb.in2p3.fr/virgo/FrameL/
----------------------------------------------------------------------


{-
compiling

hsc2hs HasKAL.FrameUtils.FrameUtils.hsc -I/opt/lscsoft/libframe-8.20/include
ghc -c HasKAL.FrameUtils.FrameUtils.hs -I/opt/lscsoft/libframe-8.20/include -L/opt/lscsoft/libframe-8.20/lib -lFrame


ghci HasKAL.FrameUtils.FrameUtils.hs -I/opt/lscsoft/libframe-8.20/include -L/opt/lscsoft/libframe-8.20/lib -lFrame

let xs :: [CFloat]
xs = [1..163840]
let sampleRate :: CDouble
sampleRate = 16384
let dt :: CDouble
dt = 10
let
let experiment_Name  :: String
    experiment_Name = "K1"
let head_Name :: String
    head_Name = "K-V"
let frametype_Name  :: String
    frametype_Name = "FrFull"
let channel_Name :: String
channel_Name = "Channel_Name"
let framefile_Name :: String
framefile_Name = "test-1065803961-10.gwf"
-}

{-# LANGUAGE CPP,  ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs #-}

module HasKAL.FrameUtils.FrameUtils
( writeFrame
, addChannel
, readFrame
, getChannelList
, getGPSTime
, getSamplingFrequency
, FrDataType(CFloatData, CDoubleData)
, eval
) where

-- Foreigns
import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array
import System.IO
import Control.Applicative
import Data.List
import Data.List.Split
import HasKAL.FrameUtils.FileManipulation


#include "FrameL.h"
-- #include "stdio.h"

type CFRVECTTYPES = (#type FRVECTTYPES)
type CFRULONG     = (#type FRULONG)
type CFRLONG      = (#type FRLONG)
newtype FrVectOption = FrVectOption { unFrVectOption :: CInt }
    deriving (Eq,Show)

#{enum FrVectOption, FrVectOption
    , frvect_c      = FR_VECT_C
    , frvect_2s     = FR_VECT_2S
    , frvect_8r     = FR_VECT_8R
    , frvect_4r     = FR_VECT_4R
    , frvect_4s     = FR_VECT_4S
    , frvect_8s     = FR_VECT_8S
    , frvect_8c     = FR_VECT_8C
    , frvect_16c    = FR_VECT_16C
    , frvect_string = FR_VECT_STRING
    , frvect_2u     = FR_VECT_2U
    , frvect_4u     = FR_VECT_4U
    , frvect_8u     = FR_VECT_8U
    , frvect_1u     = FR_VECT_1U
    , frvect_8h     = FR_VECT_8H
    , frvect_16h    = FR_VECT_16H
    , frvect_c8     = FR_VECT_C8
    , frvect_c16    = FR_VECT_C16
    , frvect_h8     = FR_VECT_H8
    , frvect_h16    = FR_VECT_H16
    , frvect_END    = FR_VECT_END}

data FrFile_partial
data FrameH_partial = FrameH_partial { frameh_dt         :: CDouble
                                     , frameh_GTimeS     :: CUInt
                                     , frameh_GTimeN     :: CUInt
                                     , frameh_frprocdata :: Ptr FrProcData_partial
                                     , frameh_frrawdata  :: Ptr C_FrRawData
                                     }
data FrProcData_partial = FrProcData_partial {frprocdata_data :: Ptr FrVect_partial}
data C_FrRawData = C_FrRawData {raw_FrAdcData :: Ptr C_FrAdcData}
data CFrameDump
data FrFile
data FrVect_partial = FrVect_partial { frvect_type    :: CFRVECTTYPES
                                     , frvect_nData   :: CFRULONG
                                     , frvect_dx      :: Ptr CDouble
                                     , frvect_nDim    :: CUInt
                                     , frvect_nx      :: Ptr CFRULONG
                                     , frvect_startX  :: CDouble
                                     , frvect_GTime   :: CDouble
                                     , frvect_dataF   :: Ptr CFloat
                                     , frvect_dataD   :: Ptr CDouble
                                     }
data C_FrAdcData = C_FrAdcData { fradc_data       :: Ptr FrVect_partial
                               , fradc_sampleRate :: CDouble
                               }
--data FrDataType  = CFloatData [CFloat] | CDoubleData [CDouble]
--                 deriving (Show)
data FrDataType a where
    CFloatData  :: [CFloat] -> FrDataType [CFloat]
    CDoubleData :: [CDouble] -> FrDataType [CDouble]
eval :: FrDataType a -> a
eval (CFloatData a)  = a
eval (CDoubleData a) = a


{-- Functions to manipulate frame-formated files --}
writeFrame :: String -> String -> String -> String -> CDouble -> CDouble -> CDouble -> [CFloat] -> IO()
writeFrame experiment_Name head_Name frametype_Name channel_Name sampleRate startGPS dt xs = do

    let nData = (truncate sampleRate)*(truncate dt) :: CFRLONG

    framefileName <- newCString experiment_Name
    frameptr <- c_FrameNew framefileName

    outputframefileName <- newCString head_Name
    frtype <- newCString frametype_Name
    oFile <- c_FrFileONewM outputframefileName 8 frtype 128

    frameptr' <- peek frameptr
    val_dt <- return $ frameh_dt frameptr'
    val_GTimeS <-  return $ frameh_GTimeS frameptr'

    channelName <- newCString channel_Name
    ptr_frprocdata <- c_FrProcDataNew frameptr channelName sampleRate nData (-32)
    -- ptr_frprocdata <- return $ frameh_frprocdata frameptr'

    frprocdata <- peek ptr_frprocdata
    ptr_frvectdata <- return $ frprocdata_data frprocdata
    frvectdata <- peek ptr_frvectdata
    ptr_frvectdataf <- return $ frvect_dataF frvectdata
    -- poke (advancePtr ptr_frvectdataf 1) (2134::CFloat)--worked, too

    let len = length xs
    ys <- withArrayLen xs $ \len ptr_xs-> do
            return ptr_xs
    ys' <- peekArray len ys
--    print . show $ take 10 ys'
    let nData' = (truncate sampleRate)*(truncate dt) :: Int
    copyArray ptr_frvectdataf ys nData'
    let startGPS' = truncate startGPS :: CUInt
        startGPSN = 0 :: CUInt
        ptr_frrawdata = nullPtr
    poke frameptr (FrameH_partial dt startGPS' startGPSN ptr_frprocdata ptr_frrawdata)

--    xs <- return (read (show [1..16384]) :: [CFloat])
--    pokeArray ptr_frvectdataf xs
-- これはうまく行かない。pokeArrayはmallocArrayなどでメモリを確保したもので
-- ないと機能しない。

--    cstdout <- ciostdout
-- 理由がまだ分からないが、Cのstdoutが使えない。
--    c_FrameDump frameptr cstdout 2
    c_FrameWrite frameptr oFile
    c_FrFileOEnd oFile
    c_FrameFree frameptr


addChannel :: CString -> String -> CDouble -> CDouble -> CDouble -> [CFloat] -> IO()
addChannel framefileName channel_Name sampleRate startGPS dt xs = do

    let nData = (truncate sampleRate)*(truncate dt) :: CFRLONG

    iFile <- c_FrFileINew framefileName
    frameptr <- c_FrameRead iFile

    channelName <- newCString channel_Name
    ptr_frprocdata <- c_FrProcDataNew frameptr channelName sampleRate nData (-32)
    -- ptr_frprocdata <- return $ frameh_frprocdata frameptr'

    frprocdata <- peek ptr_frprocdata
    ptr_frvectdata <- return $ frprocdata_data frprocdata
    frvectdata <- peek ptr_frvectdata
    ptr_frvectdataf <- return $ frvect_dataF frvectdata

    let len = length xs
    ys <- withArrayLen xs $ \len ptr_xs-> do
            return ptr_xs
    ys' <- peekArray len ys
    let nData' = (truncate sampleRate)*(truncate dt) :: Int
    copyArray ptr_frvectdataf ys nData'
    let startGPS' = truncate startGPS :: CUInt
        startGPSN = 0 :: CUInt
        ptr_frrawdata = nullPtr
    poke frameptr (FrameH_partial dt startGPS' startGPSN ptr_frprocdata ptr_frrawdata)

--    cstdout <- ciostdout
-- 理由がまだ分からないが、Cのstdoutが使えない。
--    c_FrameDump frameptr cstdout 2
    c_FrFileITEnd iFile
    c_FrameFree frameptr

{-
readFrame :: IO (String) -> IO (String) -> IO (FrDataType)
readFrame channel_Name framefile_Name = do
    let ifile :: IO (Ptr FrFile)
        ifile = framefile_Name          >>= \y ->
                newCString y            >>= \z ->
                c_FrFileINew z
    let channel = channel_Name >>= \x -> newCString x
    let fstart = ifile >>= \x -> c_FrFileITStart x
    let fend   = ifile >>= \x -> c_FrFileITEnd x
    let frlen = (-) <$> fend <*> fstart

--     let ptr_v :: Ptr FrVect_partial
    ptr_v <- ifile   >>= (\w ->
        channel     >>= (\x ->
        fstart      >>= (\y ->
        frlen       >>= (\z ->
        c_FrFileIGetV w x y z))))

    v <- peek ptr_v

    c_FrVectFree ptr_v
    ifile >>= \x -> c_FrFileIEnd x

    let datatype = frvect_type v
    case datatype of
        frvect_4r -> do
            array_vdata <- retrieveFrVectDataF v
            return (CFloatData array_vdata)
        frvect_8r -> do
            array_vdata <- retrieveFrVectDataD v
            return (CDoubleData array_vdata)
-}


readFrame :: String -> String -> IO(FrDataType [CDouble])
readFrame channel_Name framefile_Name = do

    channel <- newCString channel_Name
    framefileName <- newCString framefile_Name
    ifile <- c_FrFileINew framefileName

    fstart <- c_FrFileITStart ifile
    fend   <- c_FrFileITEnd ifile
    let frlen = fend - fstart

    ptr_v <- c_FrFileIGetV ifile channel fstart frlen
    v <- peek ptr_v

    let datatype = frvect_type v
    case datatype of
        frvect_4r -> do
          array_vdata <- peekArray (read (show (frvect_nData v)) :: Int) (frvect_dataF v)
          c_FrVectFree ptr_v
          c_FrFileIEnd ifile
--          return (CDoubleData (read (show array_vdata) :: [CDouble]))
          return (CDoubleData $ map realToFrac array_vdata)
        frvect_8r -> do
          array_vdata <- peekArray (read (show (frvect_nData v)) :: Int) (frvect_dataD v)
          c_FrVectFree ptr_v
          c_FrFileIEnd ifile
          return (CDoubleData array_vdata)




getChannelList :: String -> IO [(String, Double)]
getChannelList frameFile = do
--    let extractstartGPStime x = read $ (!!2) $ splitOn "-" $ last $ splitOn "/" x :: Int
    getChannelListCore frameFile $ extractstartGPStime frameFile

getChannelListCore :: String -> Integer -> IO [(String, Double)]
getChannelListCore frameFile gpsTime = do
    frameFile' <- newCString frameFile
    ifile <- c_FrFileINew frameFile'
    let gpsTime' = fromIntegral gpsTime :: CInt
    channelList' <- c_FrFileIGetChannelList ifile gpsTime'
    channelList'' <- peekCString channelList'
    let channelList = lines channelList''
        adcChannelList = map takeChannelandFs $ filter (isPrefixOf "ADC") channelList
        procChannelList= map takeChannelandFs $ filter (isPrefixOf "PROC") channelList
    return $ concat [procChannelList, adcChannelList]

takeChannelandFs :: String -> (String, Double)
takeChannelandFs x = (\[channelName, fs] -> (channelName, read fs :: Double)) $ splitOn "\t" $ (!!) (splitOn " " x) 1


getSamplingFrequency :: String -> String -> IO Double
getSamplingFrequency frameFile channelName = do
  getSamplingFrequencyCore frameFile channelName $ extractstartGPStime frameFile


getSamplingFrequencyCore :: String -> String -> Integer -> IO Double
getSamplingFrequencyCore frameFile channelName gpsTime = do
    channel <- newCString channelName
    framefileName <- newCString frameFile
    ifile <- c_FrFileINew framefileName
    fstart <- c_FrFileITStart ifile
    fend   <- c_FrFileITEnd ifile
    let frlen = fend - fstart
    ptr_v <- c_FrFileIGetV ifile channel fstart frlen
    v <- peek ptr_v
    dt' <- peekArray 1 (frvect_dx v)
    let dt = realToFrac (dt'!!0) :: Double
    return $ sampleRateIt dt
    where
      sampleRateIt dt
        | rate>=1.0 = fromIntegral $ floor $ rate + 0.5
        | rate<1.0  = rate
        where rate = 1.0 / dt



getGPSTime :: String -> IO (Int, Int)
getGPSTime frameFile = do
    frameFile' <- newCString frameFile
    ifile <- c_FrFileINew frameFile'
    ptr_frameH <- c_FrameRead ifile
    val_frameH <- peek ptr_frameH
    val_GTimeS <-  return $ frameh_GTimeS val_frameH
    val_GTimeN <-  return $ frameh_GTimeN val_frameH
    return (fromIntegral val_GTimeS, fromIntegral val_GTimeN)


{-- Storable Type for structure--}
instance Storable FrameH_partial where
  sizeOf = const #size FrameH
  alignment = sizeOf
  poke frameptr (FrameH_partial val_dt val_GTimeS val_GTimeN val_FrProcData val_FrRawData) = do
    (#poke FrameH, dt) frameptr val_dt
    (#poke FrameH, GTimeS) frameptr val_GTimeS
    (#poke FrameH, GTimeN) frameptr val_GTimeN
    (#poke FrameH, procData) frameptr val_FrProcData
    (#poke FrameH, rawData) frameptr val_FrRawData
  peek frameptr = do
    val_dt <- (#peek FrameH, dt) frameptr
    val_GTimeS <- (#peek FrameH, GTimeS) frameptr
    val_GTimeN <- (#peek FrameH,  GTimeN) frameptr
    val_FrProcData <- (#peek FrameH,  procData) frameptr
    val_FrRawData <- (#peek FrameH, rawData) frameptr
    return $ FrameH_partial {frameh_dt=val_dt, frameh_GTimeS=val_GTimeS
                            , frameh_GTimeN=val_GTimeN, frameh_frprocdata=val_FrProcData
                            , frameh_frrawdata=val_FrRawData}


instance Storable FrVect_partial where
  sizeOf = const #size struct FrVect
  alignment = sizeOf
  poke ptr_frvect (FrVect_partial val_type
                                  val_nData
                                  ptr_dx
                                  val_nDim
                                  ptr_nx
                                  val_startX
                                  val_GTime
                                  ptr_dataF
                                  ptr_dataD) = do
      (#poke struct FrVect, type)    ptr_frvect val_type
      (#poke struct FrVect, nData)   ptr_frvect val_nData
      (#poke struct FrVect, dx)      ptr_frvect ptr_dx
      (#poke struct FrVect, nDim)    ptr_frvect val_nDim
      (#poke struct FrVect, nx)      ptr_frvect ptr_nx
      (#poke struct FrVect, startX)  ptr_frvect val_startX
      (#poke struct FrVect, GTime)   ptr_frvect val_GTime
      (#poke struct FrVect, dataF)   ptr_frvect ptr_dataF
      (#poke struct FrVect, dataD)   ptr_frvect ptr_dataD
  peek ptr_frvect = do
      val_type    <- (#peek struct FrVect, type)      ptr_frvect
      val_nData   <- (#peek struct FrVect, nData)     ptr_frvect
      ptr_dx      <- (#peek struct FrVect, dx)        ptr_frvect
      val_nDim    <- (#peek struct FrVect, nDim)      ptr_frvect
      ptr_nx      <- (#peek struct FrVect, nx)        ptr_frvect
      val_startX  <- (#peek struct FrVect, startX)    ptr_frvect
      val_GTime   <- (#peek struct FrVect, GTime)     ptr_frvect
      ptr_dataF   <- (#peek struct FrVect, dataF)     ptr_frvect
      ptr_dataD   <- (#peek struct FrVect, dataD)     ptr_frvect
      return $ FrVect_partial { frvect_type     = val_type
                              , frvect_nData    = val_nData
                              , frvect_dx       = ptr_dx
                              , frvect_nDim     = val_nDim
                              , frvect_nx       = ptr_nx
                              , frvect_startX   = val_startX
                              , frvect_GTime    = val_GTime
                              , frvect_dataF    = ptr_dataF
                              , frvect_dataD    = ptr_dataD }


instance Storable FrProcData_partial where
  sizeOf = const #size struct FrProcData
  alignment = sizeOf
  poke frprocdataptr (FrProcData_partial ptr_data) = do
    (#poke struct FrProcData, data) frprocdataptr ptr_data
  peek frprocdataptr = do
    ptr_data <- (#peek struct FrProcData, data) frprocdataptr
    return $ FrProcData_partial {frprocdata_data = ptr_data}


instance Storable C_FrAdcData where
  sizeOf = const #size struct FrAdcData
  alignment = sizeOf
  poke ptr_FrAdcData (C_FrAdcData ptr_data val_sampleRate) = do
    (#poke struct FrAdcData, data) ptr_FrAdcData ptr_data
    (#poke struct FrAdcData, sampleRate) ptr_FrAdcData val_sampleRate
  peek ptr_FrAdcData = do
    ptr_data <- (#peek struct FrAdcData, data) ptr_FrAdcData
    val_sampleRate <- (#peek struct FrAdcData, sampleRate) ptr_FrAdcData
    return $ C_FrAdcData { fradc_data = ptr_data
                         , fradc_sampleRate = val_sampleRate
                         }




{-- import C function --}
foreign import ccall unsafe "FrameL.h FrameNew"
    c_FrameNew :: CString -> IO (Ptr FrameH_partial)

foreign import ccall unsafe "FrameL.h FrProcDataNew"
    c_FrProcDataNew :: Ptr FrameH_partial
                    -> CString
                    -> CDouble
                    -> CFRLONG
                    -> CInt
                    -> IO (Ptr FrProcData_partial)

foreign import ccall unsafe "FrameL.h FrFileONewM"
    c_FrFileONewM :: CString
                    -> CInt
                    -> CString
                    -> CInt
                    -> IO (Ptr FrFile_partial)

foreign import ccall unsafe "FrameL.h FrFileOEnd"
    c_FrFileOEnd :: Ptr FrFile_partial
                    -> IO(CInt)

foreign import ccall unsafe "FrameL.h FrameFree"
    c_FrameFree :: Ptr FrameH_partial
                    -> IO()

foreign import ccall unsafe "FrameL.h FrameWrite"
    c_FrameWrite :: Ptr FrameH_partial
                   -> Ptr FrFile_partial
                   -> IO (CInt)

foreign import ccall unsafe "FrameL.h FrameDump"
    c_FrameDump :: Ptr FrameH_partial
                   -> Ptr CFile
                   -> CInt
                   -> IO()

-- foreign import ccall unsafe "stdio.h stdout"
--    ciostdout :: IO (Ptr CFile)

foreign import ccall unsafe "FrameL.h FrFileINew"
  c_FrFileINew :: CString -> IO (Ptr FrFile)

foreign import ccall unsafe "FrameL.h FrFileIEnd"
  c_FrFileIEnd :: Ptr FrFile -> IO ()

foreign import ccall unsafe "FrameL.h FrFileITStart"
  c_FrFileITStart :: Ptr FrFile -> IO (CDouble)

foreign import ccall unsafe "FrameL.h FrFileITEnd"
  c_FrFileITEnd :: Ptr FrFile -> IO (CDouble)

foreign import ccall unsafe "FrameL.h FrFileIGetV"
  c_FrFileIGetV :: Ptr FrFile
                   -> CString
                   -> CDouble
                   -> CDouble
                   -> IO (Ptr FrVect_partial)

foreign import ccall unsafe "FrameL.h FrVectFree"
  c_FrVectFree :: Ptr FrVect_partial -> IO ()

foreign import ccall unsafe "FrameL.h FrameRead"
  c_FrameRead :: Ptr FrFile -> IO (Ptr FrameH_partial)

foreign import ccall unsafe "FrameL.h FrFileIGetChannelList"
  c_FrFileIGetChannelList :: Ptr FrFile -> CInt -> IO CString




