{-******************************************
  *     File Name: SetRangeHROOT.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/10/02 18:28:19
  *******************************************-}

module HasKAL.PlotUtils.HROOT.SetRangeHROOT (
  setXRangeUser
 ,setYRangeUser
 ,setXYRangeUser
) where

import qualified Foreign.C.Types as FCT
import qualified Foreign.Ptr as FP
import qualified Foreign.Marshal.Utils as FMU
import qualified FFICXX.Runtime.Cast as FRC
import qualified Foreign.ForeignPtr as FFP
import qualified HROOT as HR

{--  External Functions  --}
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

{--  Foreign Functions  --}
foreign import ccall "SetRangeUser" c'SetRangeUser :: FP.Ptr (FRC.Raw HR.TAxis) -> FCT.CDouble -> FCT.CDouble -> IO (FCT.CInt)
