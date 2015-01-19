{- |
Module      : HasKAL.PlotUtils.PlotUtils.HROOT.GlobalTApplication
Description : This is documentation tests.
Copyright   : (c) WhoAmI, 2014
License     : ???
Maintainer  : hoge@hoge.com
Stability   : test
Portability : POSIX

FFI TApplication Class

-}

module HasKAL.PlotUtils.HROOT.GlobalTApplication (
   gTApp
) where

import qualified Data.IORef as DIO
import qualified Foreign.C.String as FCS
import qualified Foreign.Marshal.Array as FMA
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS
import qualified HROOT as HR
import qualified System.IO.Unsafe as SIOU

{--  External Functions  --}
gTApp :: IO HR.TApplication
gTApp = DIO.readIORef $ SIOU.unsafePerformIO globalTAppIO

{--  Internal Functions  --}
globalTAppIO :: IO (DIO.IORef HR.TApplication)
globalTAppIO = do
  tApp <- HR.newTApplication (str2cstr "test") (list2ptr [toEnum 0]) $ list2ptr [str2cstr "test"]
  DIO.newIORef tApp

{--  Supplementary Functions  --}
str2cstr :: String -> FCS.CString
str2cstr = SIOU.unsafePerformIO.FCS.newCString

list2ptr :: (FS.Storable a) => [a] -> FP.Ptr a
list2ptr = SIOU.unsafePerformIO.FMA.newArray
