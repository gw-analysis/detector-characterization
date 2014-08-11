{-******************************************
  *     File Name: SignalHandlerHROOT.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/08/11 13:31:11
  *******************************************-}

module HasKAL.PlotUtils.HROOT.SignalHandlerHROOT (
  addSignalHandle
) where

import qualified HasKAL.PlotUtils.HROOT.PlotGraph as RPG

foreign import ccall "AddSignalHandle" c'AddSignalHandle :: IO ()

addSignalHandle :: IO ()
addSignalHandle = c'AddSignalHandle

