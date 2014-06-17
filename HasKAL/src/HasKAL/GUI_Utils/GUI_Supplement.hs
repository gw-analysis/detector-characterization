{-******************************************
  *     File Name: GUI_Supplement.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/06/18 02:13:03
  *******************************************-}

module HasKAL.GUI_Utils.GUI_Supplement(
   haskalOpt
  ,getActiveLabels
  ,entryNewWithLabelDefault
  ,boxPackStartDefaultsPair
  ,fileOpenButtonNewWithLabelDefault
  ,comboBoxNewLabelAppendTexts
  ,amp2psd
  ,convert_StoD
  ,convert_LtoT2
  ,convert_LtoT3
  ,convert_StoDT2L
  ,convert_StoDT3L
  ,convert_DL2S
  ,convert_DLL2S
  ,iDate2sDate
  ,transposed
) where

import Graphics.UI.Gtk
import qualified System.Environment as SE -- getEnv
import qualified System.IO.Unsafe as SIOU -- unsafePerformIO
import qualified Text.Regex as TR -- splitRegex, mkRegex
import qualified Text.Printf as TP -- printf

{-- Environment --}
haskalOpt :: String
haskalOpt = SIOU.unsafePerformIO $ SE.getEnv "HASKALOPT"

{-- Supplementary Functions --}
-- for TimeUtils
iDate2sDate :: Int -> Int -> Int -> Int -> Int -> Int -> String
iDate2sDate intYear intMonth intDay intHour intMinute intSecond =
              (TP.printf "%04d" intYear :: String) ++ "-" ++ (TP.printf "%02d" intMonth :: String) ++ "-" ++ (TP.printf "%02d" intDay :: String) ++ " " ++ (TP.printf "%02d" intHour :: String) ++ ":" ++ (TP.printf "%02d" intMinute :: String) ++ ":" ++ (TP.printf "%02d" intSecond :: String) ++ " JST"

-- for gtk
getActiveLabels :: [CheckButton] -> [String]
getActiveLabels [] = []
getActiveLabels (x:xs) = 
  case SIOU.unsafePerformIO (toggleButtonGetActive x) of 
    True -> (SIOU.unsafePerformIO (buttonGetLabel x)):(getActiveLabels xs)
    False -> getActiveLabels xs

entryNewWithDefault :: String -> IO Entry
entryNewWithDefault ss = do
  xEntry <- entryNew
  entrySetText xEntry ss
  return xEntry

entryNewWithLabelDefault :: String -> String -> IO (Label, Entry)
entryNewWithLabelDefault label defVal = do
  xLabel <- labelNewWithMnemonic label
  xEntry <- entryNewWithDefault defVal
  return (xLabel, xEntry)

boxPackStartDefaultsPair x (a, b) = do
  boxPackStartDefaults x a
  boxPackStartDefaults x b

fileOpenButtonNewWithDefault :: String -> IO FileChooserButton
fileOpenButtonNewWithDefault path = do
  fileOpener <- fileChooserButtonNew "hoge" FileChooserActionOpen
  fileChooserSetFilename fileOpener path
  return fileOpener

fileOpenButtonNewWithLabelDefault :: String -> String -> IO (Label, FileChooserButton)
fileOpenButtonNewWithLabelDefault label defPath = do
  xLabel <- labelNewWithMnemonic label
  xFileOpener <- fileOpenButtonNewWithDefault defPath
  return (xLabel, xFileOpener)

comboBoxNewAppendTexts :: [String] -> Int -> IO ComboBox
comboBoxNewAppendTexts texts defNum = do
  xComboBox <- comboBoxNewText
  mapM (comboBoxAppendText xComboBox) texts
  comboBoxSetActive xComboBox defNum
  return xComboBox

comboBoxNewLabelAppendTexts :: String -> [String] -> Int -> IO (Label, ComboBox)
comboBoxNewLabelAppendTexts label texts defNum = do
  xLabel <- labelNewWithMnemonic label
  xComboBox <- comboBoxNewAppendTexts texts defNum
  return (xLabel, xComboBox)

-- for data format
amp2psd :: (Double, Double) -> (Double, Double)
amp2psd (x, y) = (x, y*y)

convert_StoD :: String -> Double
convert_StoD = read

convert_LtoT2 :: [Double] -> (Double, Double)
convert_LtoT2 [x, y] = (x, y)
-- 列数が合わないとerrorをランタイムエラーを吐くと思うので後で修正する
convert_LtoT3 :: [Double] -> (Double, Double, Double)
convert_LtoT3 [x, y, z] = (x, y, z)

convert_StoDT2L :: String -> [(Double, Double)]
convert_StoDT2L stringData = map convert_LtoT2 (map (map convert_StoD) (map (TR.splitRegex (TR.mkRegex ",")) (lines stringData) ) )

convert_StoDT3L :: String -> [(Double, Double, Double)]
convert_StoDT3L stringData = map convert_LtoT3 (map (map convert_StoD) (map (TR.splitRegex (TR.mkRegex ",")) (lines stringData) ) )

convert_DL2S :: [Double] -> String
convert_DL2S xs = unlines $ map show xs

convert_DLL2S :: [[Double]] -> String
convert_DLL2S xss = unlines [unwords (map show xs) | xs <- xss]

transposed :: [[a]] -> [[a]]
transposed [] = []
transposed ([] : xss) = transposed xss
transposed ((x:xs) : xss) = (x : [y | (y:_) <- xss]) : transposed (xs : [z | (_:z) <- xss])
