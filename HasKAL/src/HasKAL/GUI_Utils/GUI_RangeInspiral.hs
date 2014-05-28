{-******************************************
  *     File Name: GUI_RangeInspiral.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/05/28 18:28:00
  *******************************************-}

module HasKAL.GUI_Utils.GUI_RangeInspiral(
   hasKalGuiInspiralRange
) where

import Graphics.UI.Gtk
import qualified Control.Monad as CM -- forM

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
  inspiralRangeHBoxThreshold <- hBoxNew True 5
  inspiralRangeHBoxButtons <- hBoxNew True 5

  inspiralRangeYearEntryLabel <- labelNewWithMnemonic "Year"
  inspiralRangeMonthEntryLabel <- labelNewWithMnemonic "Month"
  inspiralRangeDayEntryLabel <- labelNewWithMnemonic "Day"
  inspiralRangeHourEntryLabel <- labelNewWithMnemonic "Hour"
  inspiralRangeMinuteEntryLabel <- labelNewWithMnemonic "Minute"
  inspiralRangeSecondEntryLabel <- labelNewWithMnemonic "Second (JST)"
  inspiralRangeObsTimeEntryLabel <- labelNewWithMnemonic "OBS Time [s]"
  inspiralRangeMass1EntryLabel <- labelNewWithMnemonic "mass_min [M_solar]"
  inspiralRangeMass2EntryLabel <- labelNewWithMnemonic "mass_max [M_solar]"
  inspiralRangeThresholdEntryLabel <- labelNewWithMnemonic "Threshold SNR"

  inspiralRangeYearEntry <- entryNew
  inspiralRangeMonthEntry <- entryNew
  inspiralRangeDayEntry <- entryNew
  inspiralRangeHourEntry <- entryNew
  inspiralRangeMinuteEntry <- entryNew
  inspiralRangeSecondEntry <- entryNew
  inspiralRangeObsTimeEntry <- entryNew
  inspiralRangeMass1Entry <- entryNew
  inspiralRangeMass2Entry <- entryNew
  inspiralRangeThresholdEntry <- entryNew

  inspiralRangeClose <- buttonNewWithLabel "Close"
  inspiralRangeExecute <- buttonNewWithLabel "Execute"

  entrySetText inspiralRangeYearEntry "2013"
  entrySetText inspiralRangeMonthEntry "10"
  entrySetText inspiralRangeDayEntry "21"
  entrySetText inspiralRangeHourEntry "21"
  entrySetText inspiralRangeMinuteEntry "0"
  entrySetText inspiralRangeSecondEntry "0"
  entrySetText inspiralRangeObsTimeEntry "300"
  entrySetText inspiralRangeMass1Entry "1.0"
  entrySetText inspiralRangeMass2Entry "70.0"
  entrySetText inspiralRangeThresholdEntry "8.0"
  
  {--  Set Parameters of the objects  --}
  set inspiralRangeWindow [ windowTitle := "Inspiral Range",
                      windowDefaultWidth := 200,
                      windowDefaultHeight := 450,
                      containerChild := inspiralRangeVBox,
                      containerBorderWidth := 20 ]

  {--  Arrange object in window  --}
  boxPackStartDefaults inspiralRangeVBox inspiralRangeHBoxYear
  boxPackStartDefaults inspiralRangeHBoxYear inspiralRangeYearEntryLabel
  boxPackStartDefaults inspiralRangeHBoxYear inspiralRangeYearEntry
  boxPackStartDefaults inspiralRangeVBox inspiralRangeHBoxMonth
  boxPackStartDefaults inspiralRangeHBoxMonth inspiralRangeMonthEntryLabel
  boxPackStartDefaults inspiralRangeHBoxMonth inspiralRangeMonthEntry
  boxPackStartDefaults inspiralRangeVBox inspiralRangeHBoxDay
  boxPackStartDefaults inspiralRangeHBoxDay inspiralRangeDayEntryLabel
  boxPackStartDefaults inspiralRangeHBoxDay inspiralRangeDayEntry
  boxPackStartDefaults inspiralRangeVBox inspiralRangeHBoxHour
  boxPackStartDefaults inspiralRangeHBoxHour inspiralRangeHourEntryLabel
  boxPackStartDefaults inspiralRangeHBoxHour inspiralRangeHourEntry
  boxPackStartDefaults inspiralRangeVBox inspiralRangeHBoxMinute
  boxPackStartDefaults inspiralRangeHBoxMinute inspiralRangeMinuteEntryLabel
  boxPackStartDefaults inspiralRangeHBoxMinute inspiralRangeMinuteEntry
  boxPackStartDefaults inspiralRangeVBox inspiralRangeHBoxSecond
  boxPackStartDefaults inspiralRangeHBoxSecond inspiralRangeSecondEntryLabel
  boxPackStartDefaults inspiralRangeHBoxSecond inspiralRangeSecondEntry

  boxPackStartDefaults inspiralRangeVBox inspiralRangeHBoxObsTime
  boxPackStartDefaults inspiralRangeHBoxObsTime inspiralRangeObsTimeEntryLabel
  boxPackStartDefaults inspiralRangeHBoxObsTime inspiralRangeObsTimeEntry
  boxPackStartDefaults inspiralRangeVBox inspiralRangeHBoxMass1
  boxPackStartDefaults inspiralRangeHBoxMass1 inspiralRangeMass1EntryLabel
  boxPackStartDefaults inspiralRangeHBoxMass1 inspiralRangeMass1Entry
  boxPackStartDefaults inspiralRangeVBox inspiralRangeHBoxMass2
  boxPackStartDefaults inspiralRangeHBoxMass2 inspiralRangeMass2EntryLabel
  boxPackStartDefaults inspiralRangeHBoxMass2 inspiralRangeMass2Entry
  boxPackStartDefaults inspiralRangeVBox inspiralRangeHBoxThreshold
  boxPackStartDefaults inspiralRangeHBoxThreshold inspiralRangeThresholdEntryLabel
  boxPackStartDefaults inspiralRangeHBoxThreshold inspiralRangeThresholdEntry

  boxPackStartDefaults inspiralRangeVBox inspiralRangeHBoxButtons
  boxPackStartDefaults inspiralRangeHBoxButtons inspiralRangeClose
  boxPackStartDefaults inspiralRangeHBoxButtons inspiralRangeExecute

  {--  Execute  --}
  onClicked inspiralRangeClose $ do
    putStrLn "Closed InspiralRange Window"
    widgetDestroy inspiralRangeWindow
  onClicked inspiralRangeExecute $ do
    putStrLn "Execute"

    s_temp <- entryGetText inspiralRangeYearEntry
    let inspYear = read s_temp :: Int
    s_temp <- entryGetText inspiralRangeMonthEntry
    let inspMonth = read s_temp :: Int
    s_temp <- entryGetText inspiralRangeDayEntry
    let inspDay = read s_temp :: Int
    s_temp <- entryGetText inspiralRangeHourEntry
    let inspHour = read s_temp :: Int
    s_temp <- entryGetText inspiralRangeMinuteEntry
    let inspMinute = read s_temp :: Int
    s_temp <- entryGetText inspiralRangeSecondEntry
    let inspSecond = read s_temp :: Int

    let inspiralRangeDateStr = HGGS.iDate2sDate inspYear inspMonth inspDay inspHour inspMinute inspSecond
    putStrLn ("   JST Time: " ++ inspiralRangeDateStr)
    let hogeGPS = HTG.time2gps inspiralRangeDateStr
    putStrLn ("   GPS Time: " ++ hogeGPS)
    s_temp <- entryGetText inspiralRangeObsTimeEntry
    let inspObsTime = read s_temp :: Int
    putStrLn ("   Obs Time: " ++ (show inspObsTime) )
    s_temp <- entryGetText inspiralRangeMass1Entry
    let inspMass1 = read s_temp :: Double
    putStrLn ("     Mass_1: " ++ (show inspMass1) )
    s_temp <- entryGetText inspiralRangeMass2Entry
    let inspMass2 = read s_temp :: Double
    putStrLn ("     Mass_2: " ++ (show inspMass2) )
    s_temp <- entryGetText inspiralRangeThresholdEntry
    let inspThreshold = read s_temp :: Double
    putStrLn ("   Thresold: " ++ (show inspThreshold) )

    {-- detecter data IO and format --}
    detDataStr <- readFile $ HGGS.haskalOpt ++ "/sensitivities/bKAGRA/prebKAGRA.dat" -- ファイルは[Hz], [/rHz]が書かれているので2乗して使う
    let detData = map HGGS.amp2psd $ map HGGS.convert_LtoT2 $ map (map read) $ map words $ lines detDataStr :: [(Double, Double)]
    {-- end of detecter data IO and format --}
    {-- Monitor tool --}
    inspDist <- CM.forM [inspMass1, inspMass1+2..inspMass2] $ \mass ->
--      return $ HMRIRD.distInspiral mass mass inspThreshold detData
      return $ HMRIRD.distInspiral mass mass detData
    HPPR.hroot_core [inspMass1,inspMass1+2..inspMass2] inspDist "m1 = m2 [M_sol]" "Distance [Mpc]" HPPOR.LogXY HPPOR.Line "X11"
    {-- End of Monitor Tool --}

  {--  Exit Process  --}
  onDestroy inspiralRangeWindow mainQuit
  widgetShowAll inspiralRangeWindow
  mainGUI

