{-******************************************
  *     File Name: SignalHandlerHROOT.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/10/02 13:35:49
  *******************************************-}

module HasKAL.PlotUtils.HROOT.SignalHandlerHROOT (
  addSignalHandle
) where

import qualified Foreign.C.Types as FCT
import qualified Data.IORef as DIO
import qualified System.IO.Unsafe as SIOU

foreign import ccall "AddSignalHandle" c'AddSignalHandle :: IO (FCT.CInt)

addSignalHandle :: IO ()
addSignalHandle = do
  DIO.readIORef $ SIOU.unsafePerformIO globalSignalHandle
  return ()
-- 外部にダミー変数を見せないように `()' で返す

globalSignalHandle :: IO (DIO.IORef FCT.CInt)
globalSignalHandle = do
  dummy <- c'AddSignalHandle
  DIO.newIORef dummy
-- dummy: 複数回呼んだときに起こる不具合を回避するためのダミー変数
