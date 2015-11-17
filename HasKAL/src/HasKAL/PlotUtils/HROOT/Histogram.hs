


module HasKAL.PlotUtils.HROOT.Histogram (
   LogOption(Linear, LogX, LogY, LogXY)
  ,hist -- plot in file
  ,histX -- plot on X11
  ,oHist -- overplot in file
  ,oHistX -- overplot on X11
  ,dHist -- divide plot in file
  ,dHistX -- divide plot on X11
  ,cumulative
  ,cumulativeX
) where

import qualified Control.Monad as CM
import qualified Foreign.C.String as FCS
import qualified Foreign.C.Types as FCT
import qualified HROOT as HR
import qualified System.IO.Unsafe as SIOU

import HasKAL.PlotUtils.PlotOption.PlotOptionHROOT
import qualified HasKAL.PlotUtils.HROOT.Supplement as HRS
import qualified HasKAL.PlotUtils.HROOT.GlobalTApplication as HPG

{--  External Functions --}
hist :: LogOption -> (String, String) -> String -> String -> (Int, Double, Double) -> [Double] -> IO ()
hist log xyLabel title fname param dat = histBase Over log [xyLabel] [title] fname [param] [dat]

histX :: LogOption -> (String, String) -> String -> (Int, Double, Double) -> [Double] -> IO ()
histX log xyLabel title param dat = hist log xyLabel title "X11" param dat

oHist :: LogOption -> (String, String) -> String -> String -> [(Int, Double, Double)] -> [[Double]] -> IO ()
oHist log xyLabel title fname params dats = histBase Over log [xyLabel] (repeat title) fname params dats

oHistX :: LogOption -> (String, String) -> String -> [(Int, Double, Double)] -> [[Double]] -> IO ()
oHistX log xyLabel title params dats = oHist log xyLabel title "X11" params dats

dHist :: LogOption -> [(String, String)] -> [String] -> String -> [(Int, Double, Double)] -> [[Double]] -> IO ()
dHist log xyLabels titles fname params dats = histBase Divide log xyLabels titles fname params dats

dHistX :: LogOption -> [(String, String)] -> [String] -> [(Int, Double, Double)] -> [[Double]] -> IO ()
dHistX log xyLabels titles params dats = dHist log xyLabels titles "X11" params dats

cumulative :: LogOption -> (String, String) -> String -> String -> (Int, Double, Double) -> [Double] -> IO ()
cumulative log xyLabel title fname param dat = cumulativeBase Over log [xyLabel] [title] fname [param] [dat]

cumulativeX :: LogOption -> (String, String) -> String -> (Int, Double, Double) -> [Double] -> IO ()
cumulativeX log xyLabel title param dat = cumulative log xyLabel title "X11" param dat


{-- Internal Functions --}
histBase :: MultiPlot -> LogOption -> [(String, String)] -> [String] -> String -> [(Int, Double, Double)] -> [[Double]] -> IO ()
histBase multi log xyLabels titles fname params dats = do
  tApp <- HRS.newTApp' fname
  tCan <- HR.newTCanvas (str2cstr "title") (str2cstr "HasKAL HROOT") 640 480
  HRS.setLog' tCan log
  
  tH1ds <- CM.zipWithM newTH1D' params $ zip titles [1..(length dats)] -- titleが同じだとメモリリークを起こすので番号を付ける
  CM.zipWithM fillDat' tH1ds $ map (map realToFrac) dats
  CM.zipWithM HR.setFillColor tH1ds [2,3..]
  CM.zipWithM HR.setFillStyle tH1ds [3001,3002..]
  CM.zipWithM setXYLabel' tH1ds xyLabels

  case multi of
    Over -> do
      CM.zipWithM_ HR.draw tH1ds $ map str2cstr $ "" : repeat "same"
    Divide -> do
      HR.divide_tvirtualpad tCan 2 2 0.01 0.01 0 -- 最大4つ(2x2)に固定
      CM.forM_ [1..(min 4 $ length dats)] $ \lambda -> do
        HR.cd tCan (toEnum $ lambda)
        HR.draw (tH1ds !! (lambda-1)) (str2cstr "")

  HRS.runOrSave' tCan tApp fname

  CM.mapM_ HR.delete tH1ds
  HR.delete tCan

cumulativeBase :: MultiPlot -> LogOption -> [(String, String)] -> [String] -> String -> [(Int, Double, Double)] -> [[Double]] -> IO ()
cumulativeBase multi log xyLabels titles fname params dats = do
  tApp <- HRS.newTApp' fname
  tCan <- HR.newTCanvas (str2cstr "title") (str2cstr "HasKAL HROOT") 640 480
  HRS.setLog' tCan log
  
  tmps <- CM.zipWithM newTH1D' params $ zip (replicate (length dats) "tmp") [1..(length dats)]
  tH1ds <- CM.zipWithM newTH1D' params $ zip titles [1..(length dats)] -- titleが同じだとメモリリークを起こすので番号を付ける
  CM.zipWithM fillDat' tmps $ map (map realToFrac) dats
  CM.zipWithM hist2cumulative params (zip tmps tH1ds)
  CM.zipWithM HR.setFillColor tH1ds [2,3..]
  CM.zipWithM HR.setFillStyle tH1ds [3001,3002..]
  CM.zipWithM setXYLabel' tH1ds xyLabels

  case multi of
    Over -> do
      CM.zipWithM_ HR.draw tH1ds $ map str2cstr $ "" : repeat "same"
    Divide -> do
      HR.divide_tvirtualpad tCan 2 2 0.01 0.01 0 -- 最大4つ(2x2)に固定
      CM.forM_ [1..(min 4 $ length dats)] $ \lambda -> do
        HR.cd tCan (toEnum $ lambda)
        HR.draw (tH1ds !! (lambda-1)) (str2cstr "")

  HRS.runOrSave' tCan tApp fname

  CM.mapM_ HR.delete tH1ds
  HR.delete tCan

{-- Supplementary Functions for TH1 class --}
hist2cumulative :: (Int, Double, Double) -> (HR.TH1D, HR.TH1D) -> IO ()
hist2cumulative (nBin,_,_) (hist, cumulative) = do
  xs <- mapM (HR.getBinContent1 hist) [0..(toEnum nBin)-1] :: IO [FCT.CDouble]
  let ys = map (\n -> sum $ take n xs) [0..(length xs)-1] :: [FCT.CDouble]
  CM.zipWithM_ (HR.setBinContent1 cumulative) [0..(toEnum nBin)-1] ys

newTH1D' :: (Int, Double, Double) -> (String, Int) -> IO HR.TH1D
newTH1D' (nBin, xMin, xMax) (title, n) = HR.newTH1D (str2cstr name) (str2cstr title) (toEnum nBin) (realToFrac xMin) (realToFrac xMax)
  where name = title ++ show n -- 同じnameで複数オブジェクト生成すると

fillDat' :: (HR.ITH1 a) => a -> [FCT.CDouble] -> IO [FCT.CInt]
fillDat' a xs = CM.mapM (HR.fill1 a) xs

setXYLabel' :: (HR.ITH1 a) => a -> (String, String) -> IO ()
setXYLabel' tHist (xlabel, ylabel) = do
  HR.setXTitle tHist $ str2cstr xlabel
  HR.setYTitle tHist $ str2cstr ylabel

{-- Supplementary Functions for Foreign Types --}
str2cstr :: String -> FCS.CString
str2cstr = SIOU.unsafePerformIO.FCS.newCString

fst' :: (a, b, c) -> a
fst' (x, y, z) = x

snd' :: (a, b, c) -> b
snd' (x, y, z) = y

trd' :: (a, b, c) -> c
trd' (x, y, z) = z

