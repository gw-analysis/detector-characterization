{-******************************************
  *     File Name: GUI_Supplement.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/05/22 11:37:25
  *******************************************-}

module HasKAL.GUI_Utils.GUI_Supplement(
   getActiveLabels
  ,amp2psd
  ,convert_StoD
  ,convert_LtoT2
  ,convert_LtoT3
  ,convert_StoDT2L
  ,convert_StoDT3L
  ,convert_DLL2S
  ,iDate2sDate
  ,transposed
) where

import Graphics.UI.Gtk
import qualified System.IO.Unsafe as SIOU -- unsafePerformIO
import qualified Text.Regex as TR -- splitRegex, mkRegex
import qualified Text.Printf as TP -- printf

{-- Supplementary Functions --}
getActiveLabels :: [CheckButton] -> [String]
getActiveLabels [] = []
getActiveLabels (x:xs) = 
  case SIOU.unsafePerformIO (toggleButtonGetActive x) of 
    True -> (SIOU.unsafePerformIO (buttonGetLabel x)):(getActiveLabels xs)
    False -> getActiveLabels xs


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

convert_DLL2S :: [[Double]] -> String
convert_DLL2S xss = unlines [unwords (map show xs) | xs <- xss]

iDate2sDate :: Int -> Int -> Int -> Int -> Int -> Int -> String
iDate2sDate intYear intMonth intDay intHour intMinute intSecond =
              (TP.printf "%04d" intYear :: String) ++ "-" ++ (TP.printf "%02d" intMonth :: String) ++ "-" ++ (TP.printf "%02d" intDay :: String) ++ " " ++ (TP.printf "%02d" intHour :: String) ++ ":" ++ (TP.printf "%02d" intMinute :: String) ++ ":" ++ (TP.printf "%02d" intSecond :: String) ++ " JST"

-- transposed 2D list
-- transposed :: [[Double]] -> [[Double]]
-- transposed xxs = [ concat $ map ((drop (m-1)).(take m)) xxs | m <- [1..(length (head xxs))] ]
transposed :: [[a]] -> [[a]]
transposed [] = []
transposed ([] : xss) = transposed xss
transposed ((x:xs) : xss) = (x : [y | (y:_) <- xss]) : transposed (xs : [z | (_:z) <- xss])
-- [ [h_1(f=0), h_1(f=1), ..], [h_2(f=0), h_2(f=1), ..], ..] -> [ [h_1(f=0), h_2(f=0), ..], [h_1(f=1), h_2(f=1), ..], ..]
-- same as in RayleighMon

