{-******************************************
  *     File Name: CFunction.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/11/11 14:59:18
  *******************************************-}

module CFunction(
   modified
  ,update
) where


import qualified Foreign.C.Types as FCT
import qualified Foreign.Ptr as FP
import qualified FFICXX.Runtime.Cast as FRC
import qualified Foreign.ForeignPtr as FFP
import qualified HROOT as HR

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

foreign import ccall "cModified" c'Modified :: FP.Ptr (FRC.Raw HR.TCanvas) -> IO (FCT.CInt)
foreign import ccall "cUpdate" c'Update :: FP.Ptr (FRC.Raw HR.TCanvas) -> IO (FCT.CInt)
