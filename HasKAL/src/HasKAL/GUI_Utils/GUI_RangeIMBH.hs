{-******************************************
  *     File Name: GUI_RangeIMBH.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/05/28 18:26:47
  *******************************************-}

module HasKAL.GUI_Utils.GUI_RangeIMBH(
   hasKalGuiIMR'Range
) where

import Graphics.UI.Gtk
import qualified Control.Monad as CM -- forM

import qualified HasKAL.GUI_Utils.GUI_Supplement as HGGS
import qualified HasKAL.MonitorUtils.RangeMon.IMBH as HMRIMBHD
import qualified HasKAL.PlotUtils.PlotUtilsHROOT as HPPR
import qualified HasKAL.PlotUtils.PlotOption.PlotOptionHROOT as HPPOR
import qualified HasKAL.TimeUtils.GPSfunction as HTG

{--  Inspiral-Marger-RingdownRange Window
-- test code
main = IO ()
main = hasKalGuiIMR'Range
-- arguments: Nothing
--}
hasKalGuiIMR'Range :: IO ()
hasKalGuiIMR'Range = do
  initGUI
  putStrLn "Open Inspiral-Merger-RingdownRange Window"

  {--  Create new object  --}
  imrRangeWindow <- windowNew
  imrRangeVBox <- vBoxNew True 5
  imrRangeHBoxYear <- hBoxNew True 5
  imrRangeHBoxMonth <- hBoxNew True 5
  imrRangeHBoxDay <- hBoxNew True 5
  imrRangeHBoxHour <- hBoxNew True 5
  imrRangeHBoxMinute <- hBoxNew True 5
  imrRangeHBoxSecond <- hBoxNew True 5
  imrRangeHBoxObsTime <- hBoxNew True 5
  imrRangeHBoxMass1 <- hBoxNew True 5
  imrRangeHBoxMass2 <- hBoxNew True 5
  imrRangeHBoxButtons <- hBoxNew True 5

  imrRangeYearEntryLabel <- labelNewWithMnemonic "Year"
  imrRangeMonthEntryLabel <- labelNewWithMnemonic "Month"
  imrRangeDayEntryLabel <- labelNewWithMnemonic "Day"
  imrRangeHourEntryLabel <- labelNewWithMnemonic "Hour"
  imrRangeMinuteEntryLabel <- labelNewWithMnemonic "Minute"
  imrRangeSecondEntryLabel <- labelNewWithMnemonic "Second (JST)"
  imrRangeObsTimeEntryLabel <- labelNewWithMnemonic "OBS Time [s]"
  imrRangeMass1EntryLabel <- labelNewWithMnemonic "mass_min [M_solar]"
  imrRangeMass2EntryLabel <- labelNewWithMnemonic "mass_max [M_solar]"

  imrRangeYearEntry <- entryNew
  imrRangeMonthEntry <- entryNew
  imrRangeDayEntry <- entryNew
  imrRangeHourEntry <- entryNew
  imrRangeMinuteEntry <- entryNew
  imrRangeSecondEntry <- entryNew
  imrRangeObsTimeEntry <- entryNew
  imrRangeMass1Entry <- entryNew
  imrRangeMass2Entry <- entryNew

  imrRangeClose <- buttonNewWithLabel "Close"
  imrRangeExecute <- buttonNewWithLabel "Execute"

  entrySetText imrRangeYearEntry "2013"
  entrySetText imrRangeMonthEntry "10"
  entrySetText imrRangeDayEntry "21"
  entrySetText imrRangeHourEntry "21"
  entrySetText imrRangeMinuteEntry "0"
  entrySetText imrRangeSecondEntry "0"
  entrySetText imrRangeObsTimeEntry "300"
  entrySetText imrRangeMass1Entry "1.0"
  entrySetText imrRangeMass2Entry "300.0"

  {--  Set Parameters of the objects  --}
  set imrRangeWindow [ windowTitle := "Inspiral-Merger-Ringdown Range",
                      windowDefaultWidth := 200,
                      windowDefaultHeight := 450,
                      containerChild := imrRangeVBox,
                      containerBorderWidth := 20 ]

  {--  Arrange object in window  --}
  boxPackStartDefaults imrRangeVBox imrRangeHBoxYear
  boxPackStartDefaults imrRangeHBoxYear imrRangeYearEntryLabel
  boxPackStartDefaults imrRangeHBoxYear imrRangeYearEntry
  boxPackStartDefaults imrRangeVBox imrRangeHBoxMonth
  boxPackStartDefaults imrRangeHBoxMonth imrRangeMonthEntryLabel
  boxPackStartDefaults imrRangeHBoxMonth imrRangeMonthEntry
  boxPackStartDefaults imrRangeVBox imrRangeHBoxDay
  boxPackStartDefaults imrRangeHBoxDay imrRangeDayEntryLabel
  boxPackStartDefaults imrRangeHBoxDay imrRangeDayEntry
  boxPackStartDefaults imrRangeVBox imrRangeHBoxHour
  boxPackStartDefaults imrRangeHBoxHour imrRangeHourEntryLabel
  boxPackStartDefaults imrRangeHBoxHour imrRangeHourEntry
  boxPackStartDefaults imrRangeVBox imrRangeHBoxMinute
  boxPackStartDefaults imrRangeHBoxMinute imrRangeMinuteEntryLabel
  boxPackStartDefaults imrRangeHBoxMinute imrRangeMinuteEntry
  boxPackStartDefaults imrRangeVBox imrRangeHBoxSecond
  boxPackStartDefaults imrRangeHBoxSecond imrRangeSecondEntryLabel
  boxPackStartDefaults imrRangeHBoxSecond imrRangeSecondEntry

  boxPackStartDefaults imrRangeVBox imrRangeHBoxObsTime
  boxPackStartDefaults imrRangeHBoxObsTime imrRangeObsTimeEntryLabel
  boxPackStartDefaults imrRangeHBoxObsTime imrRangeObsTimeEntry
  boxPackStartDefaults imrRangeVBox imrRangeHBoxMass1
  boxPackStartDefaults imrRangeHBoxMass1 imrRangeMass1EntryLabel
  boxPackStartDefaults imrRangeHBoxMass1 imrRangeMass1Entry
  boxPackStartDefaults imrRangeVBox imrRangeHBoxMass2
  boxPackStartDefaults imrRangeHBoxMass2 imrRangeMass2EntryLabel
  boxPackStartDefaults imrRangeHBoxMass2 imrRangeMass2Entry

  boxPackStartDefaults imrRangeVBox imrRangeHBoxButtons
  boxPackStartDefaults imrRangeHBoxButtons imrRangeClose
  boxPackStartDefaults imrRangeHBoxButtons imrRangeExecute

  {--  Execute  --}
  onClicked imrRangeClose $ do
    putStrLn "Closed Inspiral-Merger-RingdownRange Window"
    widgetDestroy imrRangeWindow
  onClicked imrRangeExecute $ do
    putStrLn "Execute"

    s_temp <- entryGetText imrRangeYearEntry
    let imrYear = read s_temp :: Int
    s_temp <- entryGetText imrRangeMonthEntry
    let imrMonth = read s_temp :: Int
    s_temp <- entryGetText imrRangeDayEntry
    let imrDay = read s_temp :: Int
    s_temp <- entryGetText imrRangeHourEntry
    let imrHour = read s_temp :: Int
    s_temp <- entryGetText imrRangeMinuteEntry
    let imrMinute = read s_temp :: Int
    s_temp <- entryGetText imrRangeSecondEntry
    let imrSecond = read s_temp :: Int

    let imrRangeDateStr = HGGS.iDate2sDate imrYear imrMonth imrDay imrHour imrMinute imrSecond
    putStrLn ("   JST Time: " ++ imrRangeDateStr)
    let hogeGPS = HTG.time2gps imrRangeDateStr
    putStrLn ("   GPS Time: " ++ hogeGPS)
    s_temp <- entryGetText imrRangeObsTimeEntry
    let imrObsTime = read s_temp :: Int
    putStrLn ("   Obs Time: " ++ (show imrObsTime) )
    s_temp <- entryGetText imrRangeMass1Entry
    let imrMass1 = read s_temp :: Double
    putStrLn ("     Mass_1: " ++ (show imrMass1) )
    s_temp <- entryGetText imrRangeMass2Entry
    let imrMass2 = read s_temp :: Double
    putStrLn ("     Mass_2: " ++ (show imrMass2) )

    {-- detecter data IO and format --}
    detDataStr <- readFile $ HGGS.haskalOpt ++ "/sensitivities/bKAGRA/prebKAGRA.dat"
    let detData = map HGGS.amp2psd $ map HGGS.convert_LtoT2 $ map (map read) $ map words $ lines detDataStr :: [(Double, Double)]
    {-- end of detecter data IO and format --}
    {-- Monitor tool --}
    imrDist <- CM.forM [imrMass1, imrMass1+10..imrMass2] $ \mass ->
      return $ HMRIMBHD.distImbh mass mass detData
    HPPR.hroot_core [imrMass1,imrMass1+10..imrMass2] imrDist "m1 = m2 [M_sol]" "Distance [Mpc]" HPPOR.LogXY HPPOR.Line "X11"
    {-- End of Monitor Tool --}

  {--  Exit Process  --}
  onDestroy imrRangeWindow mainQuit
  widgetShowAll imrRangeWindow
  mainGUI


