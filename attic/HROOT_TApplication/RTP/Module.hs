{-******************************************
  *     File Name: Module.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/11/28 17:37:21
  *******************************************-}

module Module (
   rtPlot
  ,rtHist
  ,rtPlot3D
) where

import Data.Packed.Vector as DPV
import qualified Control.Monad as CM
import qualified System.IO.Unsafe as SIOU
import qualified Foreign.C.String as FCS
import qualified Foreign.C.Types as FCT
import HROOT

import HasKAL.PlotUtils.HROOT.Supplement as HRS
import CFunction as CF

rtHist :: (Int, Double, Double) -> [Double] -> IO ()
rtHist (nBin, xMin, xMax) dat = do
  tApp <- HRS.newTApp' "X11"
  tCan <- newTCanvas (str2cstr "title") (str2cstr "HasKAL ROOT") 640 480

  tH1d <- newTH1D (str2cstr "name") (str2cstr "title") (toEnum nBin) (realToFrac xMin) (realToFrac xMax)
  setFillColor tH1d 2
  setFillStyle tH1d 3001
  draw tH1d (str2cstr "hoge")

  CM.forM (zip [1..] dat) $ \(num, lambda) -> do
    fill1 tH1d (realToFrac lambda)
    case (mod num 1024) of
      0 -> do
        CF.modified tCan
        CF.update tCan
      _ -> return ()

  setFillColor tH1d 2
  setFillStyle tH1d 3001
  HRS.runOrSave' tCan tApp "X11"

  delete tH1d
  delete tCan

rtPlot :: [(Double, Double)] -> IO ()
rtPlot dat = do
  tApp <- HRS.newTApp' "X11"
  tCan <- newTCanvas (str2cstr "title") (str2cstr "HasKAL ROOT") 640 480

  tH1d <- newTH1D (str2cstr "Time Series") (str2cstr "title") (toEnum.length $ dat) (realToFrac.fst.head $ dat) (realToFrac.fst.last $ dat)
  draw tH1d $ str2cstr $ show "hoge"

  CM.forM (zip [1..] dat) $ \(num, lambda) -> do
    setBinContent1 tH1d (toEnum num) $ realToFrac $ (DPV.fromList $ map snd dat) @> (num -1)
    case (mod num 256) of
      0 -> do
        CF.modified tCan
        CF.update tCan
      _ -> return ()
  
  setFillColor tH1d 0
  HRS.runOrSave' tCan tApp "X11"

  delete tH1d
  delete tCan


rtPlot3D :: [(Double, Double, Double)] -> IO ()
rtPlot3D dats = do
  tApp <- HRS.newTApp' "X11"
  tCan <- newTCanvas (str2cstr "title") (str2cstr "HasKAL ROOT") 640 480

  let (xNum, yNum) = getNum dats
      (xMin, xMax, yMin, yMax) = getDataEdge dats
  tH2d <- newTH2D (str2cstr "Spectrogram") (str2cstr "title") (toEnum xNum-1) xMin xMax (toEnum yNum-1) yMin yMax

  draw tH2d $ str2cstr $ show "COLZ"

  CM.forM [1..xNum-1] $ \idxX -> do 
    CM.forM [1..yNum-1] $ \idxY ->
      setBinContent2 tH2d (toEnum idxX) (toEnum idxY) $ realToFrac $ (DPV.fromList $ map trd' dats) @> ((idxY-1) + (idxX-1) * yNum)
    CF.modified tCan
    CF.update tCan



  HRS.runOrSave' tCan tApp "X11"
  delete tH2d
  delete tCan


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

str2cstr :: String -> FCS.CString
str2cstr = SIOU.unsafePerformIO.FCS.newCString

fst' :: (a, b, c) -> a
fst' (x, y, z) = x

snd' :: (a, b, c) -> b
snd' (x, y, z) = y

trd' :: (a, b, c) -> c
trd' (x, y, z) = z
