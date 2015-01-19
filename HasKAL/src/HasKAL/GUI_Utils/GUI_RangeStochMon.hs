{- |
Module      : HasKAL.GUI_Utils.GUI_RangeStochMon
Description : This is documentation tests.
Copyright   : (c) WhoAmI, 2014
License     : ???
Maintainer  : hoge@hoge.com
Stability   : test
Portability : POSIX

GUI of Stochastic Range Monitor

-}

module HasKAL.GUI_Utils.GUI_RangeStochMon (
   hasKalGuiStochMon
) where

import Graphics.UI.Gtk
import qualified Control.Monad as CM
import qualified System.IO.Unsafe as SIOU

import qualified HasKAL.DetectorUtils.Detector as HDD
import qualified HasKAL.GUI_Utils.GUI_Supplement as HGGS
import qualified HasKAL.TimeUtils.GPSfunction as HTG
import qualified HasKAL.PlotUtils.HROOT.PlotGraph as RPG
import qualified HasKAL.MonitorUtils.RangeMon.StochMon.StochMon as RSS

{--  Test Code  --}
-- main :: IO ()
-- main = hasKalGuiStochMon

{--  Extermnal Function  --}
hasKalGuiStochMon :: IO ()
hasKalGuiStochMon = do
  initGUI
  putStrLn "Open StochMon Window"
  
  {-- Create new Object --}
  stochWindow <- windowNew
  stochVBox <- vBoxNew True 5
  stochHBoxDate <- mapM (hBoxNew True) $ take 7 [5..]
  stochHBoxObsTime <- hBoxNew True 5
  stochHBoxDet1 <- hBoxNew True 5
  stochHBoxDet2 <- hBoxNew True 5
  stochHBoxfMin <- hBoxNew True 5
  stochHBoxfMax <- hBoxNew True 5
  stochHBoxButtons <- hBoxNew True 5

  stochDateCombo <- HGGS.dateComboNew (2014, 3, 17, 16, 15, 12, "JST")
  stochObsTimeEntry <- HGGS.entryNewWithLabelDefault "OBS Time [s]" "180"
  let detectorList = map show [HDD.KAGRA, HDD.LIGO_Hanford, HDD.LIGO_Livingston, HDD.VIRGO]
  stochDet1Combo <- HGGS.comboBoxNewLabelAppendTexts "Detector 1" detectorList 0
  stochDet2Combo <- HGGS.comboBoxNewLabelAppendTexts "Detector 2" detectorList 1
  stochfMinEntry <- HGGS.entryNewWithLabelDefault "f min [Hz]" "1"
  stochfMaxEntry <- HGGS.entryNewWithLabelDefault "f max [Hz]" "1000"
  stochClose <- buttonNewWithLabel "Close"
  stochExecute <- buttonNewWithLabel "Execute"

  {-- set parameter of the objects --}
  set stochWindow [ windowTitle := "StochMon",
                      windowDefaultWidth := 200,
                      windowDefaultHeight := 450,
                      containerChild := stochVBox,
                      containerBorderWidth := 20 ]

  {-- Arrange object in window --}
  mapM (boxPackStartDefaults stochVBox) stochHBoxDate
  CM.zipWithM HGGS.boxPackStartDefaultsPair stochHBoxDate $ stochDateCombo
  boxPackStartDefaults stochVBox stochHBoxObsTime
  HGGS.boxPackStartDefaultsPair stochHBoxObsTime stochObsTimeEntry
  boxPackStartDefaults stochVBox stochHBoxDet1
  HGGS.boxPackStartDefaultsPair stochHBoxDet1 stochDet1Combo
  boxPackStartDefaults stochVBox stochHBoxDet2
  HGGS.boxPackStartDefaultsPair stochHBoxDet2 stochDet2Combo
  boxPackStartDefaults stochVBox stochHBoxfMin
  HGGS.boxPackStartDefaultsPair stochHBoxfMin stochfMinEntry
  boxPackStartDefaults stochVBox stochHBoxfMax
  HGGS.boxPackStartDefaultsPair stochHBoxfMax stochfMaxEntry
  boxPackStartDefaults stochVBox stochHBoxButtons
  mapM (boxPackStartDefaults stochHBoxButtons) [stochClose, stochExecute]

  {--  Execute --}
  onClicked stochClose $ do
    putStrLn "Close StochMon Window"
    widgetDestroy stochWindow
  onClicked stochExecute $ do
    putStrLn "Execute"
    stochDate <- CM.liftM HGGS.dateStr2Tuple $ mapM HGGS.comboBoxGetActiveString stochDateCombo
    let stochGPS = HTG.timetuple2gps stochDate
        stochObsTime = read.SIOU.unsafePerformIO.entryGetText.snd $ stochObsTimeEntry :: Int
    stochDet1 <- HGGS.comboBoxGetActiveString $ stochDet1Combo
    stochDet2 <- HGGS.comboBoxGetActiveString $ stochDet2Combo
    let stochfMin = read.SIOU.unsafePerformIO.entryGetText.snd $ stochfMinEntry :: Double
        stochfMax = read.SIOU.unsafePerformIO.entryGetText.snd $ stochfMaxEntry :: Double
    putStrLn ("    GPS Time: " ++ stochGPS)
    putStrLn ("    Obs Time: " ++ (show stochObsTime) )
    putStrLn ("  Detector 1: " ++ stochDet1 )
    putStrLn ("  Detector 2: " ++ stochDet2 )
    putStrLn ("  f min [Hz]: " ++ (show stochfMin) )
    putStrLn ("  f max [Hz]: " ++ (show stochfMax) )
    let freqs = [stochfMin..stochfMax] :: [Double]
        h2omega = RSS.h2omega_sens_allf (3*365*86400) (read stochDet1) (read stochDet2) 0.05 0.95 freqs
    RPG.plotX RPG.LogXY RPG.Line 2 RPG.BLUE ("frequency [Hz]","h2omega") 0.05 ("StochMon: ("++stochDet1++", "++stochDet2++")") ((0,0),(0,0)) $ zip freqs h2omega

  onDestroy stochWindow mainQuit
  widgetShowAll stochWindow
  mainGUI

