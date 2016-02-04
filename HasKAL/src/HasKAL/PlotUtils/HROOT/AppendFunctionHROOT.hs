


module HasKAL.PlotUtils.HROOT.AppendFunctionHROOT(
  setGrid
 ,addSignalHandle
 ,setXRangeUser
 ,setYRangeUser
 ,setXYRangeUser
 ,modified
 ,update
 ,setRangeTH2D
 ,setPadMargin
 ,setXAxisDateTGraph
 ,setXAxisDateTH2D
 ,setPallete
) where

import qualified Data.IORef as DIO
import qualified Foreign.C.Types as FCT
import qualified Foreign.Ptr as FP
import qualified Foreign.Marshal.Utils as FMU
import qualified FFICXX.Runtime.Cast as FRC
import qualified Foreign.ForeignPtr as FFP
import qualified HROOT as HR
import qualified System.IO.Unsafe as SIOU
import HasKAL.TimeUtils.GPSfunction (gps2unix)

{--  External Functions  --}
setGrid :: HR.TCanvas -> IO ()
setGrid canvas = do
  let ptr'canvas = FRC.get_fptr canvas :: FFP.ForeignPtr (FRC.Raw HR.TCanvas)
  FFP.withForeignPtr ptr'canvas c'SetGrid

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
  FFP.withForeignPtr ptr'canvas c'Modified

update :: HR.TCanvas -> IO ()
update canvas = do
  let ptr'canvas = FRC.get_fptr canvas :: FFP.ForeignPtr (FRC.Raw HR.TCanvas)
  FFP.withForeignPtr ptr'canvas c'Update

addSignalHandle :: IO ()
addSignalHandle = do
  dummy <- DIO.readIORef $ SIOU.unsafePerformIO globalSignalHandle
  return ()
-- 外部にダミー変数を見せないように `()' で返す

setRangeTH2D :: HR.TH2D -> ((Double, Double), (Double, Double)) -> IO ()
setRangeTH2D hist ((xmin, xmax), (ymin, ymax)) = do
  let ptr'hist = FRC.get_fptr hist :: FFP.ForeignPtr (FRC.Raw HR.TH2D)
  FFP.withForeignPtr ptr'hist $ \lambda -> c'SetRangeTH2D lambda (realToFrac xmin) (realToFrac xmax) (realToFrac ymin) (realToFrac ymax)

setPadMargin :: Double -> Double -> Double -> Double -> IO ()
setPadMargin l r t b = c'SetPadMargin (realToFrac l) (realToFrac r) (realToFrac t) (realToFrac b)

setXAxisDateTGraph :: HR.TGraph -> Int -> IO ()
setXAxisDateTGraph gra gps = do
  case gps >= 0 of
   True -> do
     let ptr'gra = FRC.get_fptr gra :: FFP.ForeignPtr (FRC.Raw HR.TGraph)
     FFP.withForeignPtr ptr'gra $ \lambda -> c'SetXAxisDateTGraph lambda (fromInteger $ gps2unix $ toInteger gps)
   False -> return ()

setXAxisDateTH2D :: HR.TH2D -> Int -> IO ()
setXAxisDateTH2D th2d gps = do
  case gps >= 0 of
   True -> do
     let ptr'th2d = FRC.get_fptr th2d :: FFP.ForeignPtr (FRC.Raw HR.TH2D)
     FFP.withForeignPtr ptr'th2d $ \lambda -> c'SetXAxisDateTH2D lambda (fromInteger $ gps2unix $ toInteger gps)
   False -> return ()

setPallete :: Int -> IO ()
setPallete color = c'SetPallete (fromIntegral color)


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
foreign import ccall "hSetGrid" c'SetGrid :: FP.Ptr (FRC.Raw HR.TCanvas) -> IO ()
foreign import ccall "SetRangeUser" c'SetRangeUser :: FP.Ptr (FRC.Raw HR.TAxis) -> FCT.CDouble -> FCT.CDouble -> IO ()
foreign import ccall "cModified" c'Modified :: FP.Ptr (FRC.Raw HR.TCanvas) -> IO ()
foreign import ccall "cUpdate" c'Update :: FP.Ptr (FRC.Raw HR.TCanvas) -> IO ()
foreign import ccall "AddSignalHandle" c'AddSignalHandle :: IO (FCT.CInt)
foreign import ccall "SetRangeTH" c'SetRangeTH2D :: FP.Ptr (FRC.Raw HR.TH2D) -> FCT.CDouble -> FCT.CDouble -> FCT.CDouble -> FCT.CDouble -> IO ()
foreign import ccall "SetPadMargin" c'SetPadMargin :: FCT.CDouble -> FCT.CDouble -> FCT.CDouble -> FCT.CDouble -> IO ()
foreign import ccall "SetXAxisDateTGraph" c'SetXAxisDateTGraph :: FP.Ptr (FRC.Raw HR.TGraph) -> FCT.CInt -> IO ()
foreign import ccall "SetXAxisDateTH2D" c'SetXAxisDateTH2D :: FP.Ptr (FRC.Raw HR.TH2D) -> FCT.CInt -> IO ()
foreign import ccall "SetPallete" c'SetPallete :: FCT.CInt -> IO ()
