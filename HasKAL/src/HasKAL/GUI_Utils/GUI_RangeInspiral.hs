{-******************************************
  *     File Name: GUI_RangeInspiral.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/06/18 02:35:59
  *******************************************-}

module HasKAL.GUI_Utils.GUI_RangeInspiral(
   hasKalGuiInspiralRange
) where

import Graphics.UI.Gtk
import qualified Control.Monad as CM -- forM
import qualified Data.Maybe as DM
import qualified System.IO.Unsafe as SIOU -- unsafePerformIO

import qualified HasKAL.GUI_Utils.GUI_Supplement as HGGS
import qualified HasKAL.MonitorUtils.RangeMon.InspiralRingdownDistanceQuanta as HMRIRD
import qualified HasKAL.PlotUtils.PlotOption.PlotOptionHROOT as HPPOR
import qualified HasKAL.PlotUtils.PlotUtilsHROOT as HPPR
import qualified HasKAL.TimeUtils.GPSfunction as HTG

{--  Inspiral Range Window
-- test code
main = IO ()
main = hasKalGuiInspiralRange
-- arguments: Nothing
--}
hasKalGuiInspiralRange :: IO ()
hasKalGuiInspiralRange = do
  initGUI
  putStrLn "Open InspiralRange Window"

  {--  Create new object  --}
  inspiralRangeWindow <- windowNew
  inspiralRangeVBox <- vBoxNew True 5
  inspiralRangeHBoxYear <- hBoxNew True 5
  inspiralRangeHBoxMonth <- hBoxNew True 5
  inspiralRangeHBoxDay <- hBoxNew True 5
  inspiralRangeHBoxHour <- hBoxNew True 5
  inspiralRangeHBoxMinute <- hBoxNew True 5
  inspiralRangeHBoxSecond <- hBoxNew True 5
  inspiralRangeHBoxObsTime <- hBoxNew True 5
  inspiralRangeHBoxMass1 <- hBoxNew True 5
  inspiralRangeHBoxMass2 <- hBoxNew True 5
  inspiralRangeHBoxButtons <- hBoxNew True 5

  inspiralRangeYearCombo <- HGGS.comboBoxNewLabelAppendTexts "Year" (map show [2010..2020]) 4
  inspiralRangeMonthCombo <- HGGS.comboBoxNewLabelAppendTexts "Month" (map show [1..12]) 4
  inspiralRangeDayCombo <- HGGS.comboBoxNewLabelAppendTexts "Day" (map show [1..31]) 16
  inspiralRangeHourCombo <- HGGS.comboBoxNewLabelAppendTexts "Hour" (map show [0..23]) 16
  inspiralRangeMinuteCombo <- HGGS.comboBoxNewLabelAppendTexts "Minute" (map show [0..59]) 15
  inspiralRangeSecondCombo <- HGGS.comboBoxNewLabelAppendTexts "Second (JST)" (map show [0..59]) 12
  inspiralRangeObsTimeEntry <- HGGS.entryNewWithLabelDefault "OBS Time [s]" "300"
  inspiralRangeMass1Entry <- HGGS.entryNewWithLabelDefault "mass_min [M_solor]" "1.0"
  inspiralRangeMass2Entry <- HGGS.entryNewWithLabelDefault "mass_max [M_solor]" "70.0"

  inspiralRangeClose <- buttonNewWithLabel "Close"
  inspiralRangeExecute <- buttonNewWithLabel "Execute"

  {--  Set Parameters of the objects  --}
  set inspiralRangeWindow [ windowTitle := "Inspiral Range",
                      windowDefaultWidth := 200,
                      windowDefaultHeight := 450,
                      containerChild := inspiralRangeVBox,
                      containerBorderWidth := 20 ]

  {--  Arrange object in window  --}
  boxPackStartDefaults inspiralRangeVBox inspiralRangeHBoxYear
  HGGS.boxPackStartDefaultsPair inspiralRangeHBoxYear inspiralRangeYearCombo
  boxPackStartDefaults inspiralRangeVBox inspiralRangeHBoxMonth
  HGGS.boxPackStartDefaultsPair inspiralRangeHBoxMonth inspiralRangeMonthCombo
  boxPackStartDefaults inspiralRangeVBox inspiralRangeHBoxDay
  HGGS.boxPackStartDefaultsPair inspiralRangeHBoxDay  inspiralRangeDayCombo
  boxPackStartDefaults inspiralRangeVBox inspiralRangeHBoxHour
  HGGS.boxPackStartDefaultsPair inspiralRangeHBoxHour inspiralRangeHourCombo
  boxPackStartDefaults inspiralRangeVBox inspiralRangeHBoxMinute
  HGGS.boxPackStartDefaultsPair inspiralRangeHBoxMinute inspiralRangeMinuteCombo
  boxPackStartDefaults inspiralRangeVBox inspiralRangeHBoxSecond
  HGGS.boxPackStartDefaultsPair inspiralRangeHBoxSecond inspiralRangeSecondCombo
  boxPackStartDefaults inspiralRangeVBox inspiralRangeHBoxObsTime
  HGGS.boxPackStartDefaultsPair inspiralRangeHBoxObsTime inspiralRangeObsTimeEntry
  boxPackStartDefaults inspiralRangeVBox inspiralRangeHBoxMass1
  HGGS.boxPackStartDefaultsPair inspiralRangeHBoxMass1 inspiralRangeMass1Entry
  boxPackStartDefaults inspiralRangeVBox inspiralRangeHBoxMass2
  HGGS.boxPackStartDefaultsPair inspiralRangeHBoxMass2 inspiralRangeMass2Entry

  boxPackStartDefaults inspiralRangeVBox inspiralRangeHBoxButtons
  mapM (boxPackStartDefaults inspiralRangeHBoxButtons) [inspiralRangeClose, inspiralRangeExecute]

  {--  Execute  --}
  onClicked inspiralRangeClose $ do
    putStrLn "Closed InspiralRange Window"
    widgetDestroy inspiralRangeWindow
  onClicked inspiralRangeExecute $ do
    putStrLn "Execute"
    let inspYear = read $ DM.fromJust $ SIOU.unsafePerformIO $ comboBoxGetActiveText $ snd inspiralRangeYearCombo :: Int
        inspMonth = read $ DM.fromJust $ SIOU.unsafePerformIO $ comboBoxGetActiveText $ snd inspiralRangeMonthCombo :: Int
        inspDay = read $ DM.fromJust $ SIOU.unsafePerformIO $ comboBoxGetActiveText $ snd inspiralRangeDayCombo :: Int
        inspHour = read $ DM.fromJust $ SIOU.unsafePerformIO $ comboBoxGetActiveText $ snd inspiralRangeHourCombo :: Int
        inspMinute = read $ DM.fromJust $ SIOU.unsafePerformIO $ comboBoxGetActiveText $ snd inspiralRangeMinuteCombo :: Int
        inspSecond = read $ DM.fromJust $ SIOU.unsafePerformIO $ comboBoxGetActiveText $ snd inspiralRangeSecondCombo :: Int
        inspiralRangeDateStr = HGGS.iDate2sDate inspYear inspMonth inspDay inspHour inspMinute inspSecond
        inspGPS = HTG.time2gps inspiralRangeDateStr
        inspObsTime = read $ SIOU.unsafePerformIO $ entryGetText $ snd inspiralRangeObsTimeEntry :: Int
        inspMass1 = read $ SIOU.unsafePerformIO $ entryGetText $ snd inspiralRangeMass1Entry :: Double
        inspMass2 = read $ SIOU.unsafePerformIO $ entryGetText $ snd inspiralRangeMass2Entry :: Double
    putStrLn ("   JST Time: " ++ inspiralRangeDateStr)
    putStrLn ("   GPS Time: " ++ inspGPS)
    putStrLn ("   Obs Time: " ++ (show inspObsTime) )
    putStrLn ("     Mass_1: " ++ (show inspMass1) )
    putStrLn ("     Mass_2: " ++ (show inspMass2) )

    {-- detecter data IO and format --}
    detDataStr <- readFile $ HGGS.haskalOpt ++ "/sensitivities/bKAGRA/prebKAGRA.dat" -- ファイルは[Hz], [/rHz]が書かれているので2乗して使う
    let detData = map HGGS.amp2psd $ map HGGS.convert_LtoT2 $ map (map read) $ map words $ lines detDataStr :: [(Double, Double)]
    {-- end of detecter data IO and format --}
    {-- Monitor tool --}
    inspDist <- CM.forM [inspMass1, inspMass1+2..inspMass2] $ \mass ->
      return $ HMRIRD.distInspiral mass mass detData
    HPPR.hroot_core [inspMass1,inspMass1+2..inspMass2] inspDist "m1 = m2 [M_sol]" "Distance [Mpc]" HPPOR.LogXY HPPOR.Line "X11"
    {-- End of Monitor Tool --}

  {--  Exit Process  --}
  onDestroy inspiralRangeWindow mainQuit
  widgetShowAll inspiralRangeWindow
  mainGUI

