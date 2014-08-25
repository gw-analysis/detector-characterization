{-******************************************
  *     File Name: GUI_Supplement.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/08/25 14:06:00
  *******************************************-}

module HasKAL.GUI_Utils.GUI_Supplement(
   HME.haskalOpt
  ,dateStr2Tuple
  ,getActiveLabels
  ,entryNewWithLabelDefault
  ,boxPackStartDefaultsPair
  ,fileOpenButtonNewWithLabelDefault
  ,comboBoxNewLabelAppendTexts
  ,dateComboNew
  ,amp2psd
  ,convert_StoD
  ,convert_LtoT2
  ,convert_LtoT3
  ,convert_StoDT2L
  ,convert_StoDT3L
  ,convert_DL2S
  ,convert_DLL2S
) where

import Graphics.UI.Gtk
import qualified Data.Text as DT
import qualified System.Environment as SE -- getEnv
import qualified System.IO.Unsafe as SIOU -- unsafePerformIO
import qualified Text.Regex as TR -- splitRegex, mkRegex
import qualified Text.Printf as TP -- printf

import qualified HasKAL.Misc.Environment as HME

-- ********  Move to HasKAL.Misc.Environment.hs  ***********
{-- Environment --}
-- haskalOpt :: String
-- haskalOpt = SIOU.unsafePerformIO $ SE.getEnv "HASKALOPT"
-- *********************************************************

{-- Supplementary Functions --}
-- for TimeUtils
dateStr2Tuple :: [String] -> (Int, Int, Int, Int, Int, Int, String)
dateStr2Tuple [year, month, day, hour, minute, second, tZone] =
  (read year, read month, read day, read hour, read minute, read second, tZone)

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
  mapM (comboBoxAppendText xComboBox) $ map DT.pack texts
  comboBoxSetActive xComboBox defNum
  return xComboBox

comboBoxNewLabelAppendTexts :: String -> [String] -> Int -> IO (Label, ComboBox)
comboBoxNewLabelAppendTexts label texts defNum = do
  xLabel <- labelNewWithMnemonic label
  xComboBox <- comboBoxNewAppendTexts texts defNum
  return (xLabel, xComboBox)

dateComboNew :: (Int, Int, Int, Int, Int, Int, String) -> IO [(Label, ComboBox)]
dateComboNew (year, month, day, hour, minute, second, tZone) = do
  let num = case tZone of "JST" -> 0
                          "UTC" -> 1
  comboYear <- comboBoxNewLabelAppendTexts "Year" (map show [2010..2020]) (year-2010)
  comboMonth <- comboBoxNewLabelAppendTexts "Month" (map show [1..12]) (month-1)
  comboDay <- comboBoxNewLabelAppendTexts "Day" (map show [1..31]) (day-1)
  comboHour <- comboBoxNewLabelAppendTexts "Hour" (map show [0..23]) hour
  comboMinute <- comboBoxNewLabelAppendTexts "Minute" (map show [0..59]) minute
  comboSecond <- comboBoxNewLabelAppendTexts "Second" (map show [0..59]) second
  comboTZone <- comboBoxNewLabelAppendTexts "timeZone" ["JST", "IST", "CET", "CETDST", "UTC", "CST", "CDT", "PST", "PDT"] num
  return [comboYear, comboMonth, comboDay, comboHour, comboMinute, comboSecond, comboTZone]

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
