{-******************************************
  *     File Name: GUI_RangeInspiral.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/05/30 22:22:18
  *******************************************-}

module HasKAL.GUI_Utils.GUI_RangeInspiral(
   hasKalGuiInspiralRange
) where

import Graphics.UI.Gtk
import qualified Control.Monad as CM -- forM
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

  inspiralRangeYearEntry <- HGGS.entryNewWithLabelDefault "Year" "2013"
  inspiralRangeMonthEntry <- HGGS.entryNewWithLabelDefault "Month" "10"
  inspiralRangeDayEntry <- HGGS.entryNewWithLabelDefault "Day" "21"
  inspiralRangeHourEntry <- HGGS.entryNewWithLabelDefault "Hour" "21"
  inspiralRangeMinuteEntry <- HGGS.entryNewWithLabelDefault "Minute" "0"
  inspiralRangeSecondEntry <- HGGS.entryNewWithLabelDefault "Second (JST)" "0"
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
  HGGS.boxPackStartDefaultsPair inspiralRangeHBoxYear inspiralRangeYearEntry
  boxPackStartDefaults inspiralRangeVBox inspiralRangeHBoxMonth
  HGGS.boxPackStartDefaultsPair inspiralRangeHBoxMonth inspiralRangeMonthEntry
  boxPackStartDefaults inspiralRangeVBox inspiralRangeHBoxDay
  HGGS.boxPackStartDefaultsPair inspiralRangeHBoxDay  inspiralRangeDayEntry
  boxPackStartDefaults inspiralRangeVBox inspiralRangeHBoxHour
  HGGS.boxPackStartDefaultsPair inspiralRangeHBoxHour inspiralRangeHourEntry
  boxPackStartDefaults inspiralRangeVBox inspiralRangeHBoxMinute
  HGGS.boxPackStartDefaultsPair inspiralRangeHBoxMinute inspiralRangeMinuteEntry
  boxPackStartDefaults inspiralRangeVBox inspiralRangeHBoxSecond
  HGGS.boxPackStartDefaultsPair inspiralRangeHBoxSecond inspiralRangeSecondEntry
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
    let inspYear = read $ SIOU.unsafePerformIO $ entryGetText $ snd inspiralRangeYearEntry :: Int
        inspMonth = read $ SIOU.unsafePerformIO $ entryGetText $ snd inspiralRangeMonthEntry :: Int
        inspDay = read $ SIOU.unsafePerformIO $ entryGetText $ snd inspiralRangeDayEntry :: Int
        inspHour = read $ SIOU.unsafePerformIO $ entryGetText $ snd inspiralRangeHourEntry :: Int
        inspMinute = read $ SIOU.unsafePerformIO $ entryGetText $ snd inspiralRangeMinuteEntry :: Int
        inspSecond = read $ SIOU.unsafePerformIO $ entryGetText $ snd inspiralRangeSecondEntry :: Int
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

