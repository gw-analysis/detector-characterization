{-# LINE 1 "FrameUtils.hsc" #-}
----------------------------------------------------------------------
{-# LINE 2 "FrameUtils.hsc" #-}
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



{-# LINE 66 "FrameUtils.hsc" #-}

{-# LINE 67 "FrameUtils.hsc" #-}
-- #include "stdio.h"

type CFRVECTTYPES = (Word16)
{-# LINE 70 "FrameUtils.hsc" #-}
type CFRULONG     = (Word64)
{-# LINE 71 "FrameUtils.hsc" #-}
type CFRLONG      = (Int64)
{-# LINE 72 "FrameUtils.hsc" #-}
newtype FrVectOption = FrVectOption { unFrVectOption :: CInt }
    deriving (Eq,Show)

frvect_c       :: FrVectOption
frvect_c       = FrVectOption 0
frvect_2s      :: FrVectOption
frvect_2s      = FrVectOption 1
frvect_4r      :: FrVectOption
frvect_4r      = FrVectOption 3
frvect_8r      :: FrVectOption
frvect_8r      = FrVectOption 2
frvect_4s      :: FrVectOption
frvect_4s      = FrVectOption 4
frvect_8s      :: FrVectOption
frvect_8s      = FrVectOption 5
frvect_8c      :: FrVectOption
frvect_8c      = FrVectOption 6
frvect_16c     :: FrVectOption
frvect_16c     = FrVectOption 7
frvect_string  :: FrVectOption
frvect_string  = FrVectOption 8
frvect_2u      :: FrVectOption
frvect_2u      = FrVectOption 9
frvect_4u      :: FrVectOption
frvect_4u      = FrVectOption 10
frvect_8u      :: FrVectOption
frvect_8u      = FrVectOption 11
frvect_1u      :: FrVectOption
frvect_1u      = FrVectOption 12
frvect_8h      :: FrVectOption
frvect_8h      = FrVectOption 13
frvect_16h     :: FrVectOption
frvect_16h     = FrVectOption 14
frvect_c8      :: FrVectOption
frvect_c8      = FrVectOption 6
frvect_c16     :: FrVectOption
frvect_c16     = FrVectOption 7
frvect_h8      :: FrVectOption
frvect_h8      = FrVectOption 13
frvect_h16     :: FrVectOption
frvect_h16     = FrVectOption 14
frvect_END     :: FrVectOption
frvect_END     = FrVectOption 15

{-# LINE 96 "FrameUtils.hsc" #-}

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
 --       frvect_r4 -> do
        3 -> do 
          array_vdata <- peekArray (read (show (frvect_nData v)) :: Int) (frvect_dataF v)
          c_FrVectFree ptr_v
          c_FrFileIEnd ifile
--          return (CDoubleData (read (show array_vdata) :: [CDouble]))
          return (CDoubleData $ map realToFrac array_vdata)
--        frvect_r8 -> do
        2 -> do
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
  sizeOf = const (192)
{-# LINE 347 "FrameUtils.hsc" #-}
  alignment = sizeOf
  poke frameptr (FrameH_partial val_dt val_GTimeS val_GTimeN val_FrProcData val_FrRawData) = do
    ((\hsc_ptr -> pokeByteOff hsc_ptr 40)) frameptr val_dt
{-# LINE 350 "FrameUtils.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 28)) frameptr val_GTimeS
{-# LINE 351 "FrameUtils.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 32)) frameptr val_GTimeN
{-# LINE 352 "FrameUtils.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 96)) frameptr val_FrProcData
{-# LINE 353 "FrameUtils.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 88)) frameptr val_FrRawData
{-# LINE 354 "FrameUtils.hsc" #-}
  peek frameptr = do
    val_dt <- ((\hsc_ptr -> peekByteOff hsc_ptr 40)) frameptr
{-# LINE 356 "FrameUtils.hsc" #-}
    val_GTimeS <- ((\hsc_ptr -> peekByteOff hsc_ptr 28)) frameptr
{-# LINE 357 "FrameUtils.hsc" #-}
    val_GTimeN <- ((\hsc_ptr -> peekByteOff hsc_ptr 32)) frameptr
{-# LINE 358 "FrameUtils.hsc" #-}
    val_FrProcData <- ((\hsc_ptr -> peekByteOff hsc_ptr 96)) frameptr
{-# LINE 359 "FrameUtils.hsc" #-}
    val_FrRawData <- ((\hsc_ptr -> peekByteOff hsc_ptr 88)) frameptr
{-# LINE 360 "FrameUtils.hsc" #-}
    return $ FrameH_partial {frameh_dt=val_dt, frameh_GTimeS=val_GTimeS
                            , frameh_GTimeN=val_GTimeN, frameh_frprocdata=val_FrProcData
                            , frameh_frrawdata=val_FrRawData}


instance Storable FrVect_partial where
  sizeOf = const (240)
{-# LINE 367 "FrameUtils.hsc" #-}
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
      ((\hsc_ptr -> pokeByteOff hsc_ptr 18))    ptr_frvect val_type
{-# LINE 378 "FrameUtils.hsc" #-}
      ((\hsc_ptr -> pokeByteOff hsc_ptr 24))   ptr_frvect val_nData
{-# LINE 379 "FrameUtils.hsc" #-}
      ((\hsc_ptr -> pokeByteOff hsc_ptr 64))      ptr_frvect ptr_dx
{-# LINE 380 "FrameUtils.hsc" #-}
      ((\hsc_ptr -> pokeByteOff hsc_ptr 48))    ptr_frvect val_nDim
{-# LINE 381 "FrameUtils.hsc" #-}
      ((\hsc_ptr -> pokeByteOff hsc_ptr 56))      ptr_frvect ptr_nx
{-# LINE 382 "FrameUtils.hsc" #-}
      ((\hsc_ptr -> pokeByteOff hsc_ptr 72))  ptr_frvect val_startX
{-# LINE 383 "FrameUtils.hsc" #-}
      ((\hsc_ptr -> pokeByteOff hsc_ptr 200))   ptr_frvect val_GTime
{-# LINE 384 "FrameUtils.hsc" #-}
      ((\hsc_ptr -> pokeByteOff hsc_ptr 128))   ptr_frvect ptr_dataF
{-# LINE 385 "FrameUtils.hsc" #-}
      ((\hsc_ptr -> pokeByteOff hsc_ptr 136))   ptr_frvect ptr_dataD
{-# LINE 386 "FrameUtils.hsc" #-}
  peek ptr_frvect = do
      val_type    <- ((\hsc_ptr -> peekByteOff hsc_ptr 18))      ptr_frvect
{-# LINE 388 "FrameUtils.hsc" #-}
      val_nData   <- ((\hsc_ptr -> peekByteOff hsc_ptr 24))     ptr_frvect
{-# LINE 389 "FrameUtils.hsc" #-}
      ptr_dx      <- ((\hsc_ptr -> peekByteOff hsc_ptr 64))        ptr_frvect
{-# LINE 390 "FrameUtils.hsc" #-}
      val_nDim    <- ((\hsc_ptr -> peekByteOff hsc_ptr 48))      ptr_frvect
{-# LINE 391 "FrameUtils.hsc" #-}
      ptr_nx      <- ((\hsc_ptr -> peekByteOff hsc_ptr 56))        ptr_frvect
{-# LINE 392 "FrameUtils.hsc" #-}
      val_startX  <- ((\hsc_ptr -> peekByteOff hsc_ptr 72))    ptr_frvect
{-# LINE 393 "FrameUtils.hsc" #-}
      val_GTime   <- ((\hsc_ptr -> peekByteOff hsc_ptr 200))     ptr_frvect
{-# LINE 394 "FrameUtils.hsc" #-}
      ptr_dataF   <- ((\hsc_ptr -> peekByteOff hsc_ptr 128))     ptr_frvect
{-# LINE 395 "FrameUtils.hsc" #-}
      ptr_dataD   <- ((\hsc_ptr -> peekByteOff hsc_ptr 136))     ptr_frvect
{-# LINE 396 "FrameUtils.hsc" #-}
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
  sizeOf = const (152)
{-# LINE 409 "FrameUtils.hsc" #-}
  alignment = sizeOf
  poke frprocdataptr (FrProcData_partial ptr_data) = do
    ((\hsc_ptr -> pokeByteOff hsc_ptr 16)) frprocdataptr ptr_data
{-# LINE 412 "FrameUtils.hsc" #-}
  peek frprocdataptr = do
    ptr_data <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) frprocdataptr
{-# LINE 414 "FrameUtils.hsc" #-}
    return $ FrProcData_partial {frprocdata_data = ptr_data}


instance Storable C_FrAdcData where
  sizeOf = const (120)
{-# LINE 419 "FrameUtils.hsc" #-}
  alignment = sizeOf
  poke ptr_FrAdcData (C_FrAdcData ptr_data val_sampleRate) = do
    ((\hsc_ptr -> pokeByteOff hsc_ptr 16)) ptr_FrAdcData ptr_data
{-# LINE 422 "FrameUtils.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 88)) ptr_FrAdcData val_sampleRate
{-# LINE 423 "FrameUtils.hsc" #-}
  peek ptr_FrAdcData = do
    ptr_data <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) ptr_FrAdcData
{-# LINE 425 "FrameUtils.hsc" #-}
    val_sampleRate <- ((\hsc_ptr -> peekByteOff hsc_ptr 88)) ptr_FrAdcData
{-# LINE 426 "FrameUtils.hsc" #-}
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




