{- |
Module      : HasKAL.PlotUtils.PlotUtils.HROOT.AppendFunctionHROOT
Description : This is documentation tests.
Copyright   : (c) WhoAmI, 2014
License     : ???
Maintainer  : hoge@hoge.com
Stability   : test
Portability : POSIX

FFI some functions

-}

module HasKAL.PlotUtils.HROOT.AppendFunctionHROOT(
  setGrid
 ,addSignalHandle
 ,setXRangeUser
 ,setYRangeUser
 ,setXYRangeUser
 ,modified
 ,update
 ,setRangeTH2D
) where

import qualified Data.IORef as DIO
import qualified Foreign.C.Types as FCT
import qualified Foreign.Ptr as FP
import qualified Foreign.Marshal.Utils as FMU
import qualified FFICXX.Runtime.Cast as FRC
import qualified Foreign.ForeignPtr as FFP
import qualified HROOT as HR
import qualified System.IO.Unsafe as SIOU

{--  External Functions  --}
setGrid :: HR.TCanvas -> IO ()
setGrid canvas = do
  let ptr'canvas = FRC.get_fptr canvas :: FFP.ForeignPtr (FRC.Raw HR.TCanvas)
  dummy <- FFP.withForeignPtr ptr'canvas c'SetGrid 
  return ()

setXRangeUser :: HR.TGraph -> (Double, Double) -> IO ()
setXRangeUser tGra (min, max) = do
  axis <- HR.tGraphGetXaxis tGra
  setRangeUser axis (min, max)

setYRangeUser :: HR.TGraph -> (Double, Double) -> IO ()
setYRangeUser tGra (min, max) = do
  axis <- HR.tGraphGetYaxis tGra
  setRangeUser axis (min, max)

setXYRangeUser :: HR.TGraph -> ((Double, Double), (Double, Double)) -> IO ()
setXYRangeUser tGra ((xMin, xMax), (yMin, yMax)) = do
  setXRangeUser tGra $ (xMin, xMax)
  setYRangeUser tGra $ (yMin, yMax)

modified :: HR.TCanvas -> IO ()
modified canvas = do
  let ptr'canvas = FRC.get_fptr canvas :: FFP.ForeignPtr (FRC.Raw HR.TCanvas)
  dummy <- FFP.withForeignPtr ptr'canvas c'Modified
  return ()

update :: HR.TCanvas -> IO ()
update canvas = do
  let ptr'canvas = FRC.get_fptr canvas :: FFP.ForeignPtr (FRC.Raw HR.TCanvas)
  dummy <- FFP.withForeignPtr ptr'canvas c'Update
  return ()

addSignalHandle :: IO ()
addSignalHandle = do
  DIO.readIORef $ SIOU.unsafePerformIO globalSignalHandle
  return ()
-- 外部にダミー変数を見せないように `()' で返す

setRangeTH2D :: HR.TH2D -> ((Double, Double), (Double, Double)) -> IO ()
setRangeTH2D hist ((xmin, xmax), (ymin, ymax)) = do
  let ptr'hist = FRC.get_fptr hist :: FFP.ForeignPtr (FRC.Raw HR.TH2D)
  dummy <- FFP.withForeignPtr ptr'hist $ \lambda -> c'SetRangeTH2D lambda (realToFrac xmin) (realToFrac xmax) (realToFrac ymin) (realToFrac ymax)
  return ()

{--  Internal Functions  --}
setRangeUser :: HR.TAxis -> (Double, Double) -> IO ()
setRangeUser axis (min, max) = do
  case min < max of
    True -> do
      let ptr'axis = FRC.get_fptr axis :: FFP.ForeignPtr (FRC.Raw HR.TAxis)
      dummy <- FFP.withForeignPtr ptr'axis $ \lambda -> c'SetRangeUser lambda (realToFrac min) (realToFrac max)
      return ()
    False -> do
      return ()

globalSignalHandle :: IO (DIO.IORef FCT.CInt)
globalSignalHandle = do
  dummy <- c'AddSignalHandle
  DIO.newIORef dummy
-- dummy: 複数回呼んだときに起こる不具合を回避するためのダミー変数


{--  Foreign Functions  --}
foreign import ccall "hSetGrid" c'SetGrid :: FP.Ptr (FRC.Raw HR.TCanvas) -> IO (FCT.CInt)
foreign import ccall "SetRangeUser" c'SetRangeUser :: FP.Ptr (FRC.Raw HR.TAxis) -> FCT.CDouble -> FCT.CDouble -> IO (FCT.CInt)
foreign import ccall "cModified" c'Modified :: FP.Ptr (FRC.Raw HR.TCanvas) -> IO (FCT.CInt)
foreign import ccall "cUpdate" c'Update :: FP.Ptr (FRC.Raw HR.TCanvas) -> IO (FCT.CInt)
foreign import ccall "AddSignalHandle" c'AddSignalHandle :: IO (FCT.CInt)
foreign import ccall "SetRangeTH" c'SetRangeTH2D :: FP.Ptr (FRC.Raw HR.TH2D) -> FCT.CDouble -> FCT.CDouble -> FCT.CDouble -> FCT.CDouble -> IO (FCT.CInt)

