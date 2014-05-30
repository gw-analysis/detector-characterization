{-******************************************
  *     File Name: GUI_RangeRingDown.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/05/30 22:31:01
  *******************************************-}

module HasKAL.GUI_Utils.GUI_RangeRingDown(
   hasKalGuiRingDownRange
) where

import Graphics.UI.Gtk
import qualified Control.Monad as CM -- forM
import qualified System.IO.Unsafe as SIOU -- unsafePerformIO

import qualified HasKAL.GUI_Utils.GUI_Supplement as HGGS
import qualified HasKAL.MonitorUtils.RangeMon.InspiralRingdownDistanceQuanta as HMRIRD
import qualified HasKAL.PlotUtils.PlotOption.PlotOptionHROOT as HPPOR
import qualified HasKAL.PlotUtils.PlotUtilsHROOT as HPPR
import qualified HasKAL.TimeUtils.GPSfunction as HTG

{--  RingDown Range Window
-- test code
main = IO ()
main = hasKalGuiRingDownRange
-- arguments: Nothing
--}
hasKalGuiRingDownRange :: IO ()
hasKalGuiRingDownRange = do
  initGUI
  putStrLn "Open RingDownRange Window"

  {--  Create new object  --}
  ringDownRangeWindow <- windowNew
  ringDownRangeVBox <- vBoxNew True 5
  ringDownRangeHBoxYear <- hBoxNew True 5
  ringDownRangeHBoxMonth <- hBoxNew True 5
  ringDownRangeHBoxDay <- hBoxNew True 5
  ringDownRangeHBoxHour <- hBoxNew True 5
  ringDownRangeHBoxMinute <- hBoxNew True 5
  ringDownRangeHBoxSecond <- hBoxNew True 5
  ringDownRangeHBoxObsTime <- hBoxNew True 5
  ringDownRangeHBoxMass1 <- hBoxNew True 5
  ringDownRangeHBoxMass2 <- hBoxNew True 5
  ringDownRangeHBoxButtons <- hBoxNew True 5

  ringDownRangeYearEntry <- HGGS.entryNewWithLabelDefault "Year" "2013"
  ringDownRangeMonthEntry <- HGGS.entryNewWithLabelDefault "Month" "10"
  ringDownRangeDayEntry <- HGGS.entryNewWithLabelDefault "Day" "21"
  ringDownRangeHourEntry <- HGGS.entryNewWithLabelDefault "Hour" "21"
  ringDownRangeMinuteEntry <- HGGS.entryNewWithLabelDefault "Minute" "0"
  ringDownRangeSecondEntry <- HGGS.entryNewWithLabelDefault "Second (JST)" "0"
  ringDownRangeObsTimeEntry <- HGGS.entryNewWithLabelDefault "OBS Time [s]" "300"
  ringDownRangeMass1Entry <- HGGS.entryNewWithLabelDefault "massMin [M_solar]" "10.0"
  ringDownRangeMass2Entry <- HGGS.entryNewWithLabelDefault "massMax [M_solar]" "10000.0"

  ringDownRangeClose <- buttonNewWithLabel "Close"
  ringDownRangeExecute <- buttonNewWithLabel "Execute"

  {--  Set Parameters of the objects  --}
  set ringDownRangeWindow [ windowTitle := "RingDown Range",
                      windowDefaultWidth := 200,
                      windowDefaultHeight := 450,
                      containerChild := ringDownRangeVBox,
                      containerBorderWidth := 20 ]

  {--  Arrange object in window  --}
  boxPackStartDefaults ringDownRangeVBox ringDownRangeHBoxYear
  HGGS.boxPackStartDefaultsPair ringDownRangeHBoxYear ringDownRangeYearEntry
  boxPackStartDefaults ringDownRangeVBox ringDownRangeHBoxMonth
  HGGS.boxPackStartDefaultsPair ringDownRangeHBoxMonth ringDownRangeMonthEntry
  boxPackStartDefaults ringDownRangeVBox ringDownRangeHBoxDay
  HGGS.boxPackStartDefaultsPair ringDownRangeHBoxDay ringDownRangeDayEntry
  boxPackStartDefaults ringDownRangeVBox ringDownRangeHBoxHour
  HGGS.boxPackStartDefaultsPair ringDownRangeHBoxHour ringDownRangeHourEntry
  boxPackStartDefaults ringDownRangeVBox ringDownRangeHBoxMinute
  HGGS.boxPackStartDefaultsPair ringDownRangeHBoxMinute ringDownRangeMinuteEntry
  boxPackStartDefaults ringDownRangeVBox ringDownRangeHBoxSecond
  HGGS.boxPackStartDefaultsPair ringDownRangeHBoxSecond ringDownRangeSecondEntry
  boxPackStartDefaults ringDownRangeVBox ringDownRangeHBoxObsTime
  HGGS.boxPackStartDefaultsPair ringDownRangeHBoxObsTime ringDownRangeObsTimeEntry
  boxPackStartDefaults ringDownRangeVBox ringDownRangeHBoxMass1
  HGGS.boxPackStartDefaultsPair ringDownRangeHBoxMass1 ringDownRangeMass1Entry
  boxPackStartDefaults ringDownRangeVBox ringDownRangeHBoxMass2
  HGGS.boxPackStartDefaultsPair ringDownRangeHBoxMass2 ringDownRangeMass2Entry

  boxPackStartDefaults ringDownRangeVBox ringDownRangeHBoxButtons
  mapM (boxPackStartDefaults ringDownRangeHBoxButtons) [ringDownRangeClose, ringDownRangeExecute]

  {--  Execute  --}
  onClicked ringDownRangeClose $ do
    putStrLn "Closed RingDownRange Window"
    widgetDestroy ringDownRangeWindow
  onClicked ringDownRangeExecute $ do
    putStrLn "Execute"
    let ringDYear = read $ SIOU.unsafePerformIO $ entryGetText $ snd ringDownRangeYearEntry :: Int
        ringDMonth = read $ SIOU.unsafePerformIO $ entryGetText $ snd ringDownRangeMonthEntry :: Int
        ringDDay = read $ SIOU.unsafePerformIO $ entryGetText $ snd ringDownRangeDayEntry :: Int
        ringDHour = read $ SIOU.unsafePerformIO $ entryGetText $ snd ringDownRangeHourEntry :: Int
        ringDMinute = read $ SIOU.unsafePerformIO $ entryGetText $ snd ringDownRangeMinuteEntry :: Int
        ringDSecond = read $ SIOU.unsafePerformIO $ entryGetText $ snd ringDownRangeSecondEntry :: Int
        ringDownRangeDateStr = HGGS.iDate2sDate ringDYear ringDMonth ringDDay ringDHour ringDMinute ringDSecond
        ringDGPS = HTG.time2gps ringDownRangeDateStr
        ringDObsTime = read $ SIOU.unsafePerformIO $ entryGetText $ snd ringDownRangeObsTimeEntry :: Int
        ringDMass1 = read $ SIOU.unsafePerformIO $ entryGetText $ snd ringDownRangeMass1Entry :: Double
        ringDMass2 = read $ SIOU.unsafePerformIO $ entryGetText $ snd ringDownRangeMass2Entry :: Double
    putStrLn ("   JST Time: " ++ ringDownRangeDateStr)
    putStrLn ("   GPS Time: " ++ ringDGPS)
    putStrLn ("   Obs Time: " ++ (show ringDObsTime) )
    putStrLn ("   Mass Min: " ++ (show ringDMass1) )
    putStrLn ("   Mass Max: " ++ (show ringDMass2) )

    {-- detecter data IO and format --}
    detDataStr <- readFile $ HGGS.haskalOpt ++ "/sensitivities/bKAGRA/prebKAGRA.dat"
    let detData = map HGGS.amp2psd $ map HGGS.convert_LtoT2 $ map (map read) $ map words $ lines detDataStr :: [(Double, Double)]
    {-- end of detecter data IO and format --}
    {-- Monitor tool --}
    ringDDist <- CM.forM [ringDMass1, 10.0*ringDMass1..ringDMass2] $ \mass -> 
      return $ HMRIRD.distRingdown mass detData
    HPPR.hroot_core [ringDMass1, 10.0*ringDMass1..ringDMass2] ringDDist "mass [M_sol]" "Distance [Mpc]" HPPOR.LogXY HPPOR.Line "X11"
    {-- End of Monitor Tool --}

  {--  Exit Process  --}
  onDestroy ringDownRangeWindow mainQuit
  widgetShowAll ringDownRangeWindow
  mainGUI

