{-******************************************
  *     File Name: Plot3DModuleForNewHROOT.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/08/06 16:24:19
  *******************************************-}

module Plot3DModuleForNewHROOT (
   PlotTypeOption3D(COLZ, CONTZ, LEGO2Z, AITOFF, MERCATOR)
  ,spectrogram
  ,skyMap
) where

import qualified Control.Monad as CM
import Data.Packed.Vector((@>))
import qualified Data.Packed.Vector as DPV
import qualified Foreign.C.String as FCS
import qualified Foreign.Storable as FS
import qualified Foreign.C.Types as FCT
import qualified Foreign.Ptr as FP
import qualified Foreign.Marshal.Array as FMA
import qualified HROOT as HR
import qualified System.IO.Unsafe as SIOU

import HasKAL.PlotUtils.PlotOption.PlotOptionHROOT
import qualified HROOT_Supplementary as HRS
import qualified GlobalTApplication as HPG

data PlotTypeOption3D = COLZ | CONTZ | LEGO2Z | AITOFF | MERCATOR deriving (Eq, Show)

{--  External Functions  --}
spectrogram :: LogOption -> PlotTypeOption3D -> String -> String -> String -> [(Double, Double, Double)] -> IO ()
spectrogram log mark zLabel title fname dats = plot3dBase log mark ("Time [sec]", "Frequency [Hz]", zLabel) title fname dats

skyMap :: LogOption -> PlotTypeOption3D -> String -> String -> String -> [(Double, Double, Double)] -> IO ()
skyMap log mark zLabel title fname dats = plot3dBase log mark ("longitude [deg.]", "latitude [deg.]", zLabel) title fname dats

{-- Internal Functions --}
plot3dBase :: LogOption -> PlotTypeOption3D -> (String, String, String) -> String -> String -> [(Double, Double, Double)] -> IO ()
plot3dBase log mark xyzLabel title fname dats = do
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

  HRS.runOrSave' tCan tApp fname
  HR.delete tH2d
  HR.delete tCan

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

