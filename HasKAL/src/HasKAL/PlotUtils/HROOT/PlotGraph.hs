


module HasKAL.PlotUtils.HROOT.PlotGraph (
   LogOption(Linear, LogX, LogY, LogXY)
  ,PlotTypeOption(Line, Point, LinePoint, PointLine, Dot)
  ,ColorOpt(BLACK, RED, GREEN, BLUE, YELLOW, PINK, CYAN)
  ,easyPlot
  ,easyPlotX
  ,plot -- plot in file 
  ,plotX -- plot on X11
  ,oPlot -- overplot in file
  ,oPlotX -- overplot on X11
  ,dPlot -- divide plot in file
  ,dPlotX -- divide plot on X11
  ,plotV
  ,plotXV
  ,oPlotV
  ,oPlotXV
) where

import qualified Control.Monad as CM
import qualified Data.List as DL
import qualified Foreign.C.String as FCS
import qualified Foreign.C.Types as FCT
import qualified Foreign.Marshal.Array as FMA
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS
import qualified HROOT as HR
import qualified System.IO.Unsafe as SIOU
import Data.Packed.Vector

import HasKAL.PlotUtils.PlotOption.PlotOptionHROOT
import qualified HasKAL.PlotUtils.HROOT.Supplement as HRS
import qualified HasKAL.PlotUtils.HROOT.AppendFunctionHROOT as HAF
import HasKAL.SpectrumUtils.Signature
import HasKAL.SpectrumUtils.Function

{-- External Functions --}
easyPlot :: LogOption -> String -> [(Double, Double)] -> IO ()
easyPlot log fname dat = plot log Line 2 BLUE ("x axis", "y axis") 0.05 "HasKAL Plot" fname ((0,0),(0,0)) dat

easyPlotX :: LogOption -> [(Double, Double)] -> IO ()
easyPlotX log dat = easyPlot log "X11" dat

plot :: LogOption -> PlotTypeOption -> Int -> ColorOpt -> (String, String) -> Double -> String -> String -> ((Double, Double), (Double, Double)) -> [(Double, Double)] -> IO ()
plot log mark lineWidth color xyLable labelSize title fname range dat = plotBase Over log mark lineWidth [color] [xyLable] labelSize [title] fname [range] [dat]

plotX :: LogOption -> PlotTypeOption -> Int -> ColorOpt -> (String, String) -> Double -> String -> ((Double, Double), (Double, Double)) -> [(Double, Double)] -> IO ()
plotX log mark lineWidth color xyLable labelSize title range dat = plot log mark lineWidth color xyLable labelSize title "X11" range dat

oPlot :: LogOption -> PlotTypeOption -> Int -> [ColorOpt] -> (String, String) -> Double -> String -> String -> ((Double, Double), (Double, Double)) -> [[(Double, Double)]] -> IO ()
oPlot log mark lineWidth colors xyLable labelSize title fname range dats = plotBase Over log mark lineWidth colors [xyLable] labelSize (repeat title) fname (repeat range) dats

oPlotX :: LogOption -> PlotTypeOption -> Int -> [ColorOpt] -> (String, String) -> Double -> String -> ((Double, Double), (Double, Double)) -> [[(Double, Double)]] -> IO ()
oPlotX log mark lineWidth colors xyLable labelSize title range dats = oPlot log mark lineWidth colors xyLable labelSize title "X11" range dats

dPlot :: LogOption -> PlotTypeOption -> Int -> [ColorOpt] -> [(String, String)] -> Double -> [String] -> String -> [((Double, Double), (Double, Double))] -> [[(Double, Double)]] -> IO ()
dPlot log mark lineWidth colors xyLables labelSize titles fname ranges dats = plotBase Divide log mark lineWidth colors xyLables labelSize titles fname ranges dats
  
dPlotX :: LogOption -> PlotTypeOption -> Int -> [ColorOpt] -> [(String, String)] -> Double -> [String] -> [((Double, Double), (Double, Double))] -> [[(Double, Double)]] -> IO ()
dPlotX log mark lineWidth colors xyLables labelSize titles ranges dats = dPlot log mark lineWidth colors xyLables labelSize titles "X11" ranges dats


plotV :: LogOption -> PlotTypeOption -> Int -> ColorOpt -> (String, String) -> Double -> String -> String -> ((Double, Double), (Double, Double)) -> Spectrum -> IO ()
plotV log mark lineWidth color xyLable labelSize title fname range dat = plotBaseV Over log mark lineWidth [color] [xyLable] labelSize [title] fname [range] [dat]

plotXV :: LogOption -> PlotTypeOption -> Int -> ColorOpt -> (String, String) -> Double -> String -> ((Double, Double), (Double, Double)) -> Spectrum -> IO ()
plotXV log mark lineWidth color xyLable labelSize title range dat = plotV log mark lineWidth color xyLable labelSize title "X11" range dat

oPlotV :: LogOption -> PlotTypeOption -> Int -> [ColorOpt] -> (String, String) -> Double -> String -> String -> ((Double, Double), (Double, Double)) -> [Spectrum] -> IO ()
oPlotV log mark lineWidth colors xyLable labelSize title fname range dats = plotBaseV Over log mark lineWidth colors [xyLable] labelSize (repeat title) fname (repeat range) dats

oPlotXV :: LogOption -> PlotTypeOption -> Int -> [ColorOpt] -> (String, String) -> Double -> String -> ((Double, Double), (Double, Double)) -> [Spectrum] -> IO ()
oPlotXV log mark lineWidth colors xyLable labelSize title range dats = oPlotV log mark lineWidth colors xyLable labelSize title "X11" range dats


{-- Internal Functions --}
defColors :: [ColorOpt]
defColors = [BLACK, RED, GREEN, BLUE, YELLOW, PINK, CYAN]

plotBase :: MultiPlot -> LogOption -> PlotTypeOption -> Int -> [ColorOpt] -> [(String, String)] -> Double -> [String] -> String -> [((Double, Double), (Double, Double))] -> [[(Double, Double)]] -> IO ()
plotBase multi log mark lineWidth colors xyLables labelSize titles fname ranges dats = do
  tApp <- HRS.newTApp' fname
  tCan <- HR.newTCanvas (str2cstr "title") (str2cstr "HasKAL ROOT") 640 480
  HAF.setGrid tCan
  HRS.setLog' tCan log
  HAF.setPadMargin 0.15 0.15 1 1

  tGras <- CM.forM dats $ \dat -> HR.newTGraph (toEnum $ length dat) (getPtrX dat) (getPtrY dat)
  CM.zipWithM_ HR.setTitle tGras $ map str2cstr titles -- title
  setColors' tGras $ DL.nub $ colors ++ defColors --[2,3..] -- Line, Markerの色(赤, 緑, 青,...に固定)
  mapM (flip HR.setLineWidth $ fromIntegral lineWidth) tGras
  CM.zipWithM_ setXYLabel' tGras xyLables -- lable (X軸、Y軸)
  CM.zipWithM_ HAF.setXYRangeUser tGras ranges -- range (X軸, Y軸)
  mapM (flip setLabelSize' labelSize) tGras -- font size

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

plotBaseV :: MultiPlot -> LogOption -> PlotTypeOption -> Int -> [ColorOpt] -> [(String, String)] -> Double -> [String] -> String -> [((Double, Double), (Double, Double))] -> [Spectrum] -> IO ()
plotBaseV multi log mark lineWidth colors xyLables labelSize titles fname ranges dats = do
  tApp <- HRS.newTApp' fname
  tCan <- HR.newTCanvas (str2cstr "title") (str2cstr "HasKAL ROOT") 640 480
  HAF.setGrid tCan
  HRS.setLog' tCan log
  HAF.setPadMargin 0.15 0.15 1 1

  tGras <- CM.forM dats $ \(freqV, specV) -> HR.newTGraph (toEnum $ dim specV) (list2ptr $ map realToFrac $ toList freqV) (list2ptr $ map realToFrac $ toList specV)
  CM.zipWithM_ HR.setTitle tGras $ map str2cstr titles -- title
  setColors' tGras $ DL.nub $ colors ++ defColors --[2,3..] -- Line, Markerの色(赤, 緑, 青,...に固定)
  mapM (flip HR.setLineWidth $ fromIntegral lineWidth) tGras
  CM.zipWithM_ setXYLabel' tGras xyLables -- lable (X軸、Y軸)
  CM.zipWithM_ HAF.setXYRangeUser tGras ranges -- range (X軸, Y軸)
  mapM (flip setLabelSize' labelSize) tGras -- font size

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

setColors' :: [HR.TGraph] -> [ColorOpt] -> IO [()]
setColors' tGras colors = do
  CM.zipWithM HR.setLineColor tGras $ map color2cint colors
  CM.zipWithM HR.setMarkerColor tGras $ map color2cint colors

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

setLabelSize' :: HR.TGraph -> Double -> IO ()
setLabelSize' tGra size = do
  xAxis <- HR.tGraphGetXaxis tGra
  yAxis <- HR.tGraphGetYaxis tGra
  mapM_ (flip HR.setLabelSize $ realToFrac size) [xAxis, yAxis]
  mapM_ (flip HR.setTitleSize $ realToFrac size) [xAxis, yAxis]

{-- Supplementary Functions for Foreign Types --}
str2cstr :: String -> FCS.CString
str2cstr = SIOU.unsafePerformIO.FCS.newCString

list2ptr :: (FS.Storable a) => [a] -> FP.Ptr a
list2ptr = SIOU.unsafePerformIO.FMA.newArray

getPtrX :: [(Double, Double)] -> FP.Ptr FCT.CDouble
getPtrX = list2ptr.map (realToFrac.fst)

getPtrY :: [(Double, Double)] -> FP.Ptr FCT.CDouble
getPtrY = list2ptr.map (realToFrac.snd)
