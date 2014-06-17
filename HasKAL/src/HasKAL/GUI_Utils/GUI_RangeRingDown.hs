{-******************************************
  *     File Name: GUI_RangeRingDown.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/06/18 02:39:44
  *******************************************-}

module HasKAL.GUI_Utils.GUI_RangeRingDown(
   hasKalGuiRingDownRange
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

  ringDownRangeYearCombo <- HGGS.comboBoxNewLabelAppendTexts "Year" (map show [2010..2020]) 4
  ringDownRangeMonthCombo <- HGGS.comboBoxNewLabelAppendTexts "Month" (map show [1..12]) 4
  ringDownRangeDayCombo <- HGGS.comboBoxNewLabelAppendTexts "Day" (map show [1..31]) 16
  ringDownRangeHourCombo <- HGGS.comboBoxNewLabelAppendTexts "Hour" (map show [0..23]) 16
  ringDownRangeMinuteCombo <- HGGS.comboBoxNewLabelAppendTexts "Minute" (map show [0..59]) 15
  ringDownRangeSecondCombo <- HGGS.comboBoxNewLabelAppendTexts "Second (JST)" (map show [0..59]) 12
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
  HGGS.boxPackStartDefaultsPair ringDownRangeHBoxYear ringDownRangeYearCombo
  boxPackStartDefaults ringDownRangeVBox ringDownRangeHBoxMonth
  HGGS.boxPackStartDefaultsPair ringDownRangeHBoxMonth ringDownRangeMonthCombo
  boxPackStartDefaults ringDownRangeVBox ringDownRangeHBoxDay
  HGGS.boxPackStartDefaultsPair ringDownRangeHBoxDay ringDownRangeDayCombo
  boxPackStartDefaults ringDownRangeVBox ringDownRangeHBoxHour
  HGGS.boxPackStartDefaultsPair ringDownRangeHBoxHour ringDownRangeHourCombo
  boxPackStartDefaults ringDownRangeVBox ringDownRangeHBoxMinute
  HGGS.boxPackStartDefaultsPair ringDownRangeHBoxMinute ringDownRangeMinuteCombo
  boxPackStartDefaults ringDownRangeVBox ringDownRangeHBoxSecond
  HGGS.boxPackStartDefaultsPair ringDownRangeHBoxSecond ringDownRangeSecondCombo
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
    let ringDYear = read $ DM.fromJust $ SIOU.unsafePerformIO $ comboBoxGetActiveText $ snd ringDownRangeYearCombo :: Int
        ringDMonth = read $ DM.fromJust $ SIOU.unsafePerformIO $ comboBoxGetActiveText $ snd ringDownRangeMonthCombo :: Int
        ringDDay = read $ DM.fromJust $ SIOU.unsafePerformIO $ comboBoxGetActiveText $ snd ringDownRangeDayCombo :: Int
        ringDHour = read $ DM.fromJust $ SIOU.unsafePerformIO $ comboBoxGetActiveText $ snd ringDownRangeHourCombo :: Int
        ringDMinute = read $ DM.fromJust $ SIOU.unsafePerformIO $ comboBoxGetActiveText $ snd ringDownRangeMinuteCombo :: Int
        ringDSecond = read $ DM.fromJust $ SIOU.unsafePerformIO $ comboBoxGetActiveText $ snd ringDownRangeSecondCombo :: Int
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

