{-# HADDOCK Markdown #-}
{- |
Module      : HasKAL.PlotUtils.PlotUtils.HROOT.PlotGraph3D
Description : This is documentation tests.
Copyright   : (c) WhoAmI, 2014
License     : ???
Maintainer  : hoge@hoge.com
Stability   : test
Portability : POSIX

3D plot tool

-}

module HasKAL.PlotUtils.HROOT.PlotGraph3D (
   LogOption(Linear, LogX, LogY, LogZ, LogXY, LogXZ, LogYZ, LogXYZ)
  ,PlotTypeOption3D(COLZ, CONTZ, LEGO2Z, AITOFF, MERCATOR)
  ,spectrogram
  ,spectrogramX
  ,skyMap
  ,skyMapX
  ,spectrogramM
  ,spectrogramMX
  ,skyMapM
  ,skyMapMX
) where

import qualified Control.Monad as CM
import Data.Packed.Vector((@>))
import qualified Data.Packed.Vector as DPV
import Data.Packed.Matrix((@@>),(><))
import qualified Data.Packed.Matrix as DPM
import qualified Foreign.C.String as FCS
import qualified Foreign.Storable as FS
import qualified Foreign.C.Types as FCT
import qualified Foreign.Ptr as FP
import qualified Foreign.Marshal.Array as FMA
import qualified HROOT as HR
import qualified System.IO.Unsafe as SIOU

import qualified HasKAL.Misc.CoastData as MCD
import HasKAL.PlotUtils.PlotOption.PlotOptionHROOT
import qualified HasKAL.PlotUtils.HROOT.Supplement as HRS
import qualified HasKAL.PlotUtils.HROOT.GlobalTApplication as HPG

import HasKAL.SpectrumUtils.Signature
import HasKAL.SpectrumUtils.Function

data OptBG = NONE | COAST deriving (Eq)

{--  External Functions  --}
spectrogram :: LogOption -> PlotTypeOption3D -> String -> String -> String -> [(Double, Double, Double)] -> IO ()
spectrogram log mark zLabel title fname dats = plot3dBase NONE log mark ("Time [sec]", "Frequency [Hz]", zLabel) title fname dats

spectrogramX :: LogOption -> PlotTypeOption3D -> String -> String -> [(Double, Double, Double)] -> IO ()
spectrogramX log mark zLabel title dats = spectrogram log mark zLabel title "X11" dats

skyMap :: LogOption -> PlotTypeOption3D -> String -> String -> String -> [(Double, Double, Double)] -> IO ()
skyMap log mark zLabel title fname dats = plot3dBase COAST log mark ("longitude [deg.]", "latitude [deg.]", zLabel) title fname dats

skyMapX :: LogOption -> PlotTypeOption3D -> String -> String -> [(Double, Double, Double)] -> IO ()
skyMapX log mark zLabel title dats = skyMap log mark zLabel title "X11" dats

spectrogramM :: LogOption -> PlotTypeOption3D -> String -> String -> String -> Spectrogram -> IO ()
spectrogramM log mark zLabel title fname dats = plot3dBaseM NONE log mark ("Time [sec]", "Frequency [Hz]", zLabel) title fname dats

spectrogramMX :: LogOption -> PlotTypeOption3D -> String -> String -> Spectrogram -> IO ()
spectrogramMX log mark zLabel title dats = spectrogramM log mark zLabel title "X11" dats

skyMapM :: LogOption -> PlotTypeOption3D -> String -> String -> String -> Spectrogram -> IO ()
skyMapM log mark zLabel title fname dats = plot3dBaseM COAST log mark ("longitude [deg.]", "latitude [deg.]", zLabel) title fname dats

skyMapMX :: LogOption -> PlotTypeOption3D -> String -> String -> Spectrogram -> IO ()
skyMapMX log mark zLabel title dats = skyMapM log mark zLabel title "X11" dats

{-- Internal Functions --}
plot3dBase :: OptBG -> LogOption -> PlotTypeOption3D -> (String, String, String) -> String -> String -> [(Double, Double, Double)] -> IO ()
plot3dBase wmap log mark xyzLabel title fname dats = do
  tApp <- HRS.newTApp' fname
  tCan <- HR.newTCanvas (str2cstr "hoge") (str2cstr "HasKAL") 640 480
  HRS.setLog' tCan log
    
  let (xNum, yNum) = getNum dats
      (xMin, xMax, yMin, yMax) = getDataEdge dats
  tH2d <- HR.newTH2D (str2cstr "Spectrogram") (str2cstr title) (toEnum xNum-1) xMin xMax (toEnum yNum-1) yMin yMax
  CM.forM [1..xNum-1] $ \idxX -> CM.forM [1..yNum-1] $ \idxY ->
    HR.setBinContent2 tH2d (toEnum idxX) (toEnum idxY) $ realToFrac $ (DPV.fromList $ map trd' dats) @> ((idxY-1) + (idxX-1) * yNum)
  setXYZLabel' tH2d xyzLabel
  HR.setStats tH2d 0

  HR.draw tH2d $ str2cstr $ show mark
  case wmap of NONE  -> return ()
               COAST -> oPlotCoastLine

  HRS.runOrSave' tCan tApp fname
  HR.delete tH2d
  HR.delete tCan

plot3dBaseM :: OptBG -> LogOption -> PlotTypeOption3D -> (String, String, String) -> String -> String -> Spectrogram -> IO ()
plot3dBaseM wmap log mark xyzLabel title fname (tV, fV, specM) = do
  tApp <- HRS.newTApp' fname
  tCan <- HR.newTCanvas (str2cstr "hoge") (str2cstr "HasKAL") 640 480
  HRS.setLog' tCan log
    
  let (xNum, yNum) = (DPV.dim tV, DPV.dim fV)
      (xMin, xMax, yMin, yMax) = (realToFrac $ tV@>0, realToFrac $ tV@>(xNum-1), realToFrac $ fV@>0, realToFrac $ fV@>(yNum-1))
  tH2d <- HR.newTH2D (str2cstr "Spectrogram") (str2cstr title) (toEnum xNum-1) xMin xMax (toEnum yNum-1) yMin yMax
  CM.forM [1..xNum-1] $ \idxX -> CM.forM [1..yNum-1] $ \idxY ->
    HR.setBinContent2 tH2d (toEnum idxX) (toEnum idxY) $ realToFrac $ specM @@> (idxY-1,idxX-1)
  setXYZLabel' tH2d xyzLabel
  HR.setStats tH2d 0

  HR.draw tH2d $ str2cstr $ show mark
  case wmap of NONE  -> return ()
               COAST -> oPlotCoastLine

  HRS.runOrSave' tCan tApp fname
  HR.delete tH2d
  HR.delete tCan

oPlotCoastLine :: IO ()
oPlotCoastLine = do
  let cl = MCD.coast
  tGra <- HR.newTGraph (toEnum $ length cl) (list2ptr $ map (realToFrac.fst) cl) (list2ptr $ map (realToFrac.snd) cl)
  HR.setMarkerColor tGra $ toEnum 0
  HR.draw tGra $ str2cstr "P"

getNum :: [(Double, Double, Double)] -> (Int, Int)
getNum dats = (xNum, yNum)
  where xNum = length dats `div` yNum
        yNum = length $ filter (== head xs) xs
        xs = map fst' dats

getDataEdge :: [(Double, Double, Double)] -> (FCT.CDouble, FCT.CDouble, FCT.CDouble, FCT.CDouble)
getDataEdge dats = (xMin, xMax, yMin, yMax)
  where xMin = realToFrac $ fst' dath
        xMax = realToFrac $ fst' datl
        yMin = realToFrac $ snd' dath
        yMax = realToFrac $ snd' datl
        dath = head dats
        datl = last dats

{-- Supplementary Functions for TH --}
setXYZLabel' :: HR.ITH1 a => a -> (String, String, String) -> IO ()
setXYZLabel' thist (xName, yName, zName) = do
  HR.setXTitle thist $ str2cstr xName
  HR.setYTitle thist $ str2cstr yName
  HR.setZTitle thist $ str2cstr zName

{--  Supplementary Functions for Foreign Types  --}
str2cstr :: String -> FCS.CString
str2cstr = SIOU.unsafePerformIO.FCS.newCString

list2ptr :: (FS.Storable a) => [a] -> FP.Ptr a
list2ptr = SIOU.unsafePerformIO.FMA.newArray

fst' :: (a, b, c) -> a
fst' (x, y, z) = x

snd' :: (a, b, c) -> b
snd' (x, y, z) = y

trd' :: (a, b, c) -> c
trd' (x, y, z) = z

        