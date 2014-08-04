{-******************************************
  *     File Name: PlotModuleForNewHROOT.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/08/02 01:39:23
  *******************************************-}

module PlotModuleForNewHROOT (
   LogOption(Linear, LogX, LogY, LogXY)
  ,PlotTypeOption(Line, Point, LinePoint, PointLine, Dot)
  ,plot -- plot in file 
  ,plotX -- plot on X11
  ,oPlot -- overplot in file
  ,oPlotX -- overplot on X11
  ,dPlot -- divide plot in file
  ,dPlotX -- divide plot on X11
) where

import qualified Control.Monad as CM
import qualified Data.IORef as DIO
import qualified Data.Maybe as DM
import qualified HROOT as HR
import qualified System.IO.Unsafe as SIOU
import qualified Foreign.C.String as FCS
import qualified Foreign.C.Types as FCT
import qualified Foreign.Marshal.Array as FMA
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS

import HasKAL.PlotUtils.PlotOption.PlotOptionHROOT

data MultiPlot = Over | Divide deriving (Eq)

{-- External Functions --}
plot :: LogOption -> PlotTypeOption -> (String, String) -> String -> [(Double, Double)] -> IO ()
plot log mark (labelX, labelY) fname dat = oPlot log mark (labelX, labelY) fname [dat]

plotX :: LogOption -> PlotTypeOption -> (String, String) -> [(Double, Double)] -> IO ()
plotX log mark (labelX, labelY) dat = oPlot log mark (labelX, labelY) "X11" [dat]

oPlot :: LogOption -> PlotTypeOption -> (String, String) -> String -> [[(Double, Double)]] -> IO ()
oPlot log mark (labelX, labelY) fname dats = plotBase Over log mark (labelX, labelY) fname dats

oPlotX :: LogOption -> PlotTypeOption -> (String, String) -> [[(Double, Double)]] -> IO ()
oPlotX log mark (labelX, labelY) dats = oPlot log mark (labelX, labelY) "X11" dats

dPlot :: LogOption -> PlotTypeOption -> (String, String) -> String -> [[(Double, Double)]] -> IO ()
dPlot log mark (labelX, labelY) fname dats = plotBase Divide log mark (labelX, labelY) fname dats
  
dPlotX :: LogOption -> PlotTypeOption -> (String, String) -> [[(Double, Double)]] -> IO ()
dPlotX log mark (labelX, labelY) dats = dPlot log mark (labelX, labelY) "X11" dats


{-- Internal Functions --}
plotBase :: MultiPlot -> LogOption -> PlotTypeOption -> (String, String) -> String -> [[(Double, Double)]] -> IO ()
plotBase multi log mark (labelX, labelY) fname dats = do
  tApp <- case fname of 
    "X11" -> CM.liftM DM.Just $ DIO.readIORef gTApp
    _     -> return DM.Nothing
  tCan <- HR.newTCanvas (str2cstr "title") (str2cstr "window name") 640 480
  setLog' tCan log

  tGras <- CM.forM dats $ \dat -> HR.newTGraph (toEnum $ length dat) (list2ptr $ map cfst dat) (list2ptr $ map csnd dat)
  case multi of
    Over -> draw' tGras mark
    Divide -> do
      HR.divide_tvirtualpad tCan 2 2 0.01 0.01 0 -- 後ろ3つの引数が不明
      CM.liftM concat $ CM.forM [1..(min 4 $ length dats)] $ \lambda -> do
        HR.cd tCan (toEnum $ lambda)
        draw' [tGras !! (lambda-1)] mark

  case fname of 
    "X11" -> HR.run (DM.fromJust tApp) 1
    _     -> HR.saveAs tCan (str2cstr fname) (str2cstr "")

  CM.mapM HR.delete tGras
  HR.delete tCan

setLog' :: HR.TCanvas -> LogOption -> IO ()
setLog' tCan flag
  | flag == Linear = return ()
  | flag == LogX   = HR.setLogx tCan 1
  | flag == LogY   = HR.setLogy tCan 1
  -- | flag == LogZ   = HR.setLogz tCan 1
  | flag == LogXY  = mapM_ (setLog' tCan) [LogX, LogY]
  -- | flag == LogXZ  = mapM_ (setLog' tCan) [LogX, LogZ]
  -- | flag == LogYZ  = mapM_ (setLog' tCan) [LogY, LogZ]
  -- | flag == LogXYZ  = mapM_ (setLog' tCan) [LogX, LogY, LogZ]

draw' :: [HR.TGraph] -> PlotTypeOption -> IO [()]
draw' tGra flag
  | flag == Line      = CM.zipWithM HR.draw tGra $ (str2cstr "AL") : repeat (str2cstr "L")
  | flag == Point     = CM.zipWithM HR.draw tGra $ (str2cstr "AP*") : repeat (str2cstr "P*")
  | flag == LinePoint = CM.zipWithM HR.draw tGra $ (str2cstr "ALP*") : repeat (str2cstr "LP*")
  | flag == PointLine = CM.zipWithM HR.draw tGra $ (str2cstr "ALP*") : repeat (str2cstr "LP*")
  | flag == Dot       = CM.zipWithM HR.draw tGra $ (str2cstr "AP") : repeat (str2cstr "P")

-- for TApplication class as global var.
gTApp :: DIO.IORef HR.TApplication
gTApp = SIOU.unsafePerformIO globalTAppIO

globalTAppIO :: IO (DIO.IORef HR.TApplication)
globalTAppIO = do
  tApp <- HR.newTApplication (str2cstr "test") (list2ptr [toEnum 0]) $ list2ptr [str2cstr "test"]
  DIO.newIORef tApp

{-- Supplementary Functions --}
str2cstr :: String -> FCS.CString
str2cstr = SIOU.unsafePerformIO.FCS.newCString

list2ptr :: (FS.Storable a) => [a] -> FP.Ptr a
list2ptr = SIOU.unsafePerformIO.FMA.newArray

cfst :: (Double, Double) -> FCT.CDouble
cfst = realToFrac.fst

csnd :: (Double, Double) -> FCT.CDouble
csnd = realToFrac.snd