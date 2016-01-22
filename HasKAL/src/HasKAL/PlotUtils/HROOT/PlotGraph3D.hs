


module HasKAL.PlotUtils.HROOT.PlotGraph3D (
   LogOption(Linear, LogX, LogY, LogZ, LogXY, LogXZ, LogYZ, LogXYZ)
  ,PlotTypeOption3D(COLZ, CONTZ, LEGO2Z, AITOFF, MERCATOR)
  ,histgram2d
  ,histgram2dX
  ,spectrogram
  ,spectrogramX
  ,skyMap
  ,skyMapX
  ,histgram2dM
  ,histgram2dMX
  ,spectrogramM
  ,spectrogramMX
  ,skyMapM
  ,skyMapMX
  ,histgram2dDateM
  ,histgram2dDateMX
  ,unsafePlot3d
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
import qualified HasKAL.PlotUtils.HROOT.AppendFunctionHROOT as HAF
import HasKAL.Misc.StrictMapping (forM')
import HasKAL.SpectrumUtils.Signature
import HasKAL.SpectrumUtils.Function

data OptBG = NONE | COAST deriving (Eq)

{--  External Functions  --}
histgram2d :: LogOption -> PlotTypeOption3D -> (String, String, String) -> String -> String -> ((Double, Double), (Double, Double)) -> [(Double, Double, Double)] -> IO ()
histgram2d log mark (xl, yl, zl) title fname range dat = plot3dBase NONE log mark (xl, yl, zl) title fname range dat

histgram2dX :: LogOption -> PlotTypeOption3D -> (String, String, String) -> String -> ((Double, Double), (Double, Double)) -> [(Double, Double, Double)] -> IO ()
histgram2dX log mark (xl, yl, zl) title range dat = plot3dBase NONE log mark (xl, yl, zl) title "X11" range dat

spectrogram :: LogOption -> PlotTypeOption3D -> String -> String -> String -> ((Double, Double), (Double, Double)) -> [(Double, Double, Double)] -> IO ()
spectrogram log mark zLabel title fname range dats = plot3dBase NONE log mark ("Time [s]", "Frequency [Hz]", zLabel) title fname range dats

spectrogramX :: LogOption -> PlotTypeOption3D -> String -> String -> ((Double, Double), (Double, Double)) -> [(Double, Double, Double)] -> IO ()
spectrogramX log mark zLabel title range dats = spectrogram log mark zLabel title "X11" range dats

skyMap :: LogOption -> PlotTypeOption3D -> String -> String -> String -> ((Double, Double), (Double, Double)) -> [(Double, Double, Double)] -> IO ()
skyMap log mark zLabel title fname range dats = plot3dBase COAST log mark ("longitude [deg.]", "latitude [deg.]", zLabel) title fname range dats

skyMapX :: LogOption -> PlotTypeOption3D -> String -> String -> ((Double, Double), (Double, Double)) -> [(Double, Double, Double)] -> IO ()
skyMapX log mark zLabel title range dats = skyMap log mark zLabel title "X11" range dats

histgram2dM :: LogOption -> PlotTypeOption3D -> (String, String, String) -> String -> String -> ((Double, Double), (Double, Double)) -> Spectrogram -> IO ()
histgram2dM log mark (xl, yl, zl) title fname range dat = plot3dBaseM NONE log mark (xl, yl, zl) title fname range (-1) dat

histgram2dMX :: LogOption -> PlotTypeOption3D -> (String, String, String) -> String -> ((Double, Double), (Double, Double)) -> Spectrogram -> IO ()
histgram2dMX log mark (xl, yl, zl) title range dat = plot3dBaseM NONE log mark (xl, yl, zl) title "X11" range (-1) dat

spectrogramM :: LogOption -> PlotTypeOption3D -> String -> String -> String -> ((Double, Double), (Double, Double)) -> Spectrogram -> IO ()
spectrogramM log mark zLabel title fname range dats = plot3dBaseM NONE log mark ("Time [s]", "Frequency [Hz]", zLabel) title fname range (-1) dats

spectrogramMX :: LogOption -> PlotTypeOption3D -> String -> String -> ((Double, Double), (Double, Double)) -> Spectrogram -> IO ()
spectrogramMX log mark zLabel title range dats = spectrogramM log mark zLabel title "X11" range dats

skyMapM :: LogOption -> PlotTypeOption3D -> String -> String -> String -> ((Double, Double), (Double, Double)) -> Spectrogram -> IO ()
skyMapM log mark zLabel title fname range dats = plot3dBaseM COAST log mark ("longitude [deg.]", "latitude [deg.]", zLabel) title fname range (-1) dats

skyMapMX :: LogOption -> PlotTypeOption3D -> String -> String -> ((Double, Double), (Double, Double)) -> Spectrogram -> IO ()
skyMapMX log mark zLabel title range dats = skyMapM log mark zLabel title "X11" range dats

histgram2dDateM :: LogOption -> PlotTypeOption3D -> (String, String, String) -> String -> String -> ((Double, Double), (Double, Double)) -> Int -> Spectrogram -> IO ()
histgram2dDateM log mark (xl, yl, zl) title fname range gps dat = plot3dBaseM NONE log mark (xl, yl, zl) title fname range gps dat

histgram2dDateMX :: LogOption -> PlotTypeOption3D -> (String, String, String) -> String -> ((Double, Double), (Double, Double)) -> Int -> Spectrogram -> IO ()
histgram2dDateMX log mark (xl, yl, zl) title range gps dat = plot3dBaseM NONE log mark (xl, yl, zl) title "X11" range gps dat

{-- Internal Functions --}
plot3dBase :: OptBG -> LogOption -> PlotTypeOption3D -> (String, String, String) -> String -> String -> ((Double, Double), (Double, Double)) -> [(Double, Double, Double)] -> IO ()
plot3dBase wmap log mark xyzLabel title fname range dats = do
  tApp <- HRS.newTApp' fname
  tCan <- HR.newTCanvas (str2cstr "hoge") (str2cstr "HasKAL") 640 480
  HRS.setLog' tCan log
  HAF.setPadMargin 0.15 0.15 1 1

  let (xNum, yNum) = getNum dats
      (xMin, xMax, yMin, yMax) = getDataEdge dats
  tH2d <- HR.newTH2D (str2cstr "Spectrogram") (str2cstr title) (toEnum xNum-1) xMin xMax (toEnum yNum-1) yMin yMax
  CM.forM [1..xNum-1] $ \idxX -> CM.forM [1..yNum-1] $ \idxY ->
    HR.setBinContent2 tH2d (toEnum idxX) (toEnum idxY) $ realToFrac $ (DPV.fromList $ map trd' dats) @> ((idxY-1) + (idxX-1) * yNum)
  setXYZLabel' tH2d xyzLabel
  HR.setStats tH2d 0
  HAF.setRangeTH2D tH2d range

  HR.draw tH2d $ str2cstr $ show mark
  case wmap of NONE  -> return ()
               COAST -> oPlotCoastLine

  HRS.runOrSave' tCan tApp fname
  HR.delete tH2d
  HR.delete tCan

plot3dBaseM :: OptBG -> LogOption -> PlotTypeOption3D -> (String, String, String) -> String -> String -> ((Double, Double), (Double, Double)) -> Int -> Spectrogram -> IO ()
plot3dBaseM wmap log mark xyzLabel title fname range gps (tV, fV, specM) = do
  tApp <- HRS.newTApp' fname
  tCan <- HR.newTCanvas (str2cstr "hoge") (str2cstr "HasKAL") 640 480
  HRS.setLog' tCan log
  HAF.setPadMargin 0.15 0.15 1 1

  let (xNum, yNum) = (DPV.dim tV, DPV.dim fV)
      (xMin, xMax, yMin, yMax) = (realToFrac $ tV@>0, realToFrac $ tV@>(xNum-1), realToFrac $ fV@>0, realToFrac $ fV@>(yNum-1))
  tH2d <- HR.newTH2D (str2cstr "Spectrogram") (str2cstr title) (toEnum xNum-1) xMin xMax (toEnum yNum-1) yMin yMax
  HAF.setXAxisDateTH2D tH2d gps
  forM' [1..xNum-1] $ \idxX -> forM' [1..yNum-1] $ \idxY ->
    HR.setBinContent2 tH2d (toEnum idxX) (toEnum idxY) $ realToFrac $ specM @@> (idxY-1,idxX-1)
  setXYZLabel' tH2d xyzLabel
  HR.setStats tH2d 0
  HAF.setRangeTH2D tH2d range

  HR.draw tH2d $ str2cstr $ show mark
  case wmap of NONE  -> return ()
               COAST -> oPlotCoastLine

  HRS.runOrSave' tCan tApp fname
  HR.delete tH2d
  HR.delete tCan

unsafePlot3d :: ((Int,Int), (Int,Int)) -> LogOption -> PlotTypeOption3D -> [(String, String, String)] -> [String] -> String -> [((Double, Double), (Double, Double))] -> [Spectrogram] -> IO ()
unsafePlot3d ((r,rd),(c,cd)) log mark xyzLabels titles fname ranges dats = do
  tApp <- HRS.newTApp' fname
  tCan <- HR.newTCanvas (str2cstr "hoge") (str2cstr "HasKAL") (toEnum rd) (toEnum cd)
  HRS.setLog' tCan log
  HAF.setPadMargin 0.15 0.15 1 1

  let idx = minimum [length xyzLabels, length titles, length ranges, length dats]
  tH2ds <- CM.forM [0,1..idx-1] $ \idxI -> do
    let (tV, fV, specM) = dats !! idxI
        xyzLabel = xyzLabels !! idxI
        title = titles !! idxI
        range = ranges !! idxI
    let (xNum, yNum) = (DPV.dim tV, DPV.dim fV)
        (xMin, xMax, yMin, yMax) = (realToFrac $ tV@>0, realToFrac $ tV@>(xNum-1), realToFrac $ fV@>0, realToFrac $ fV@>(yNum-1))
    tH2d <- HR.newTH2D (str2cstr ("2dHist"++":"++(show idxI))) (str2cstr title) (toEnum xNum-1) xMin xMax (toEnum yNum-1) yMin yMax
    forM' [1..xNum-1] $ \idxX -> forM' [1..yNum-1] $ \idxY ->
      HR.setBinContent2 tH2d (toEnum idxX) (toEnum idxY) $ realToFrac $ specM @@> (idxY-1,idxX-1)
    setXYZLabel' tH2d xyzLabel
    HR.setStats tH2d 0
    HAF.setRangeTH2D tH2d range
    return tH2d

  case idx of
   1 -> do
     HR.draw (tH2ds!!0) $ str2cstr $ show mark
   _ -> do
     HR.divide_tvirtualpad tCan (toEnum r) (toEnum c) 0.01 0.01 0
     CM.forM_ [1..(min idx $ r*c)] $ \lambda -> do
        HR.cd tCan (toEnum $ lambda)
        HR.draw (tH2ds!!(lambda-1)) $ str2cstr $ show mark
     
  HRS.runOrSave' tCan tApp fname
  CM.mapM HR.delete tH2ds
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


