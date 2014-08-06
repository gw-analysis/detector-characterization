{-******************************************
  *     File Name: PlotModuleForNewHROOT.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/08/06 10:50:48
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
import qualified Foreign.C.String as FCS
import qualified Foreign.C.Types as FCT
import qualified Foreign.Marshal.Array as FMA
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS
import qualified HROOT as HR
import qualified System.IO.Unsafe as SIOU

import HasKAL.PlotUtils.PlotOption.PlotOptionHROOT
import qualified HROOT_Supplementary as HRS
import qualified GlobalTApplication as HPG

data MultiPlot = Over | Divide deriving (Eq)
data ColorOpt = BLACK | RED | GREEN | BLUE | YELLOW | PINK | CYAN deriving (Eq, Ord)

{-- External Functions --}
plot :: LogOption -> PlotTypeOption -> (String, String) -> String -> String -> [(Double, Double)] -> IO ()
plot log mark xyLable title fname dat = oPlot log mark [xyLable] [title] fname [dat]

plotX :: LogOption -> PlotTypeOption -> (String, String) -> String -> [(Double, Double)] -> IO ()
plotX log mark xyLable title dat = oPlot log mark [xyLable] [title] "X11" [dat]

oPlot :: LogOption -> PlotTypeOption -> [(String, String)] -> [String] -> String -> [[(Double, Double)]] -> IO ()
oPlot log mark xyLables titles fname dats = plotBase Over log mark xyLables titles fname dats

oPlotX :: LogOption -> PlotTypeOption -> [(String, String)] -> [String] -> [[(Double, Double)]] -> IO ()
oPlotX log mark xyLables titles dats = oPlot log mark xyLables titles "X11" dats

dPlot :: LogOption -> PlotTypeOption -> [(String, String)] -> [String] -> String -> [[(Double, Double)]] -> IO ()
dPlot log mark xyLables titles fname dats = plotBase Divide log mark xyLables titles fname dats
  
dPlotX :: LogOption -> PlotTypeOption -> [(String, String)] -> [String] -> [[(Double, Double)]] -> IO ()
dPlotX log mark xyLables titles dats = dPlot log mark xyLables titles "X11" dats


{-- Internal Functions --}
plotBase :: MultiPlot -> LogOption -> PlotTypeOption -> [(String, String)] -> [String] -> String -> [[(Double, Double)]] -> IO ()
plotBase multi log mark xyLables titles fname dats = do
  tApp <- HRS.newTApp' fname
  tCan <- HR.newTCanvas (str2cstr "title") (str2cstr "HasKAL ROOT") 640 480
  HRS.setLog' tCan log

  tGras <- CM.forM dats $ \dat -> HR.newTGraph (toEnum $ length dat) (getPtrX dat) (getPtrY dat)
  CM.zipWithM_ HR.setTitle tGras $ map str2cstr titles -- title
  setColors' tGras [2,3..] -- Line, Markerの色(赤, 緑, 青,...に固定)
  mapM (flip HR.setLineWidth 2) tGras -- Lineの太さ(2に固定)
  CM.zipWithM_ setXYLabel' tGras xyLables -- lable (X軸、Y軸)

  case multi of -- :: IO ()
    Over -> do 
      draws' tGras mark
    Divide -> do
      HR.divide_tvirtualpad tCan 2 2 0.01 0.01 0 -- 最大4つ(2x2)に固定
      CM.forM_ [1..(min 4 $ length dats)] $ \lambda -> do
        HR.cd tCan (toEnum $ lambda)
        draws' [tGras !! (lambda-1)] mark

  HRS.runOrSave' tCan tApp fname
  CM.mapM HR.delete tGras
  HR.delete tCan


{--  Supplementary Functions for TGraph --}
draws' :: [HR.TGraph] -> PlotTypeOption -> IO ()
draws' tGras flag
  | flag == Line      = CM.zipWithM_ HR.draw tGras $ map str2cstr $ "AL" : repeat "L"
  | flag == Point     = CM.zipWithM_ HR.draw tGras $ map str2cstr $ "AP*" : repeat "P*"
  | flag == LinePoint = CM.zipWithM_ HR.draw tGras $ map str2cstr $ "ALP*" : repeat "LP*"
  | flag == PointLine = CM.zipWithM_ HR.draw tGras $ map str2cstr $ "ALP*" : repeat "LP*"
  | flag == Dot       = CM.zipWithM_ HR.draw tGras $ map str2cstr $ "AP" : repeat "P"

setColors' :: [HR.TGraph] -> [Int] -> IO [()]
setColors' tGras colors = do
  CM.zipWithM HR.setLineColor tGras $ map toEnum colors
  CM.zipWithM HR.setMarkerColor tGras $ map toEnum colors

setXYLabel' :: HR.TGraph -> (String, String) -> IO ()
setXYLabel' tGra (labelX, labelY) = do
  HR.setTitle (SIOU.unsafePerformIO $ HR.tGraphGetXaxis tGra) $ str2cstr labelX
  HR.setTitle (SIOU.unsafePerformIO $ HR.tGraphGetYaxis tGra) $ str2cstr labelY

color2cint :: ColorOpt -> FCT.CInt
color2cint color
  | color == BLACK  = 1
  | color == RED    = 2
  | color == GREEN  = 3
  | color == BLUE   = 4
  | color == YELLOW = 5
  | color == PINK   = 6
  | color == CYAN   = 7

{-- Supplementary Functions for Foreign Types --}
str2cstr :: String -> FCS.CString
str2cstr = SIOU.unsafePerformIO.FCS.newCString

list2ptr :: (FS.Storable a) => [a] -> FP.Ptr a
list2ptr = SIOU.unsafePerformIO.FMA.newArray

getPtrX :: [(Double, Double)] -> FP.Ptr FCT.CDouble
getPtrX = list2ptr.map (realToFrac.fst)

getPtrY :: [(Double, Double)] -> FP.Ptr FCT.CDouble
getPtrY = list2ptr.map (realToFrac.snd)
