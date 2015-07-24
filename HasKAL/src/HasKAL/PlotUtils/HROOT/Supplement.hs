


module HasKAL.PlotUtils.HROOT.Supplement (
   setLog'
  ,newTApp'
  ,runOrSave'
) where

import qualified Control.Monad as CM
import qualified Data.Maybe as DM
import qualified Foreign.C.String as FCS
import qualified HROOT as HR
import qualified System.IO.Unsafe as SIOU

import HasKAL.PlotUtils.PlotOption.PlotOptionHROOT
import qualified HasKAL.PlotUtils.HROOT.GlobalTApplication as HPG

{--  External Functions  --}
setLog' :: HR.TCanvas -> LogOption -> IO ()
setLog' tCan flag
  | flag == Linear = return ()
  | flag == LogX   = HR.setLogx tCan 1
  | flag == LogY   = HR.setLogy tCan 1
  | flag == LogZ   = HR.setLogz tCan 1
  | flag == LogXY  = mapM_ (setLog' tCan) [LogX, LogY]
  | flag == LogXZ  = mapM_ (setLog' tCan) [LogX, LogZ]
  | flag == LogYZ  = mapM_ (setLog' tCan) [LogY, LogZ]
  | flag == LogXYZ  = mapM_ (setLog' tCan) [LogX, LogY, LogZ]

newTApp' :: String -> IO (Maybe HR.TApplication)
newTApp' fname = case fname of
  "X11" -> CM.liftM DM.Just HPG.gTApp
  _     -> return DM.Nothing

runOrSave' :: HR.TCanvas -> Maybe HR.TApplication ->  String -> IO ()
runOrSave' tCan tApp fname = case fname of
  "X11" -> HR.run (DM.fromJust tApp) 1
  _     -> HR.saveAs tCan (str2cstr fname) (str2cstr "")


{-- Supplementary Functions for Foreign Types --}
str2cstr :: String -> FCS.CString
str2cstr = SIOU.unsafePerformIO.FCS.newCString
