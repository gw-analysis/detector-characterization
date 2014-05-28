{-******************************************
  *     File Name: GUI_RangeRingDown.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/05/28 18:28:38
  *******************************************-}

module HasKAL.GUI_Utils.GUI_RangeRingDown(
   hasKalGuiRingDownRange
) where

import Graphics.UI.Gtk
import qualified Control.Monad as CM -- forM

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
  ringDownRangeHBoxMass <- hBoxNew True 5
  ringDownRangeHBoxKerrParam <- hBoxNew True 5
  ringDownRangeHBoxMassDefect <- hBoxNew True 5
  ringDownRangeHBoxIniPhase <- hBoxNew True 5
  ringDownRangeHBoxThreshold <- hBoxNew True 5
  ringDownRangeHBoxButtons <- hBoxNew True 5

  ringDownRangeYearEntryLabel <- labelNewWithMnemonic "Year"
  ringDownRangeMonthEntryLabel <- labelNewWithMnemonic "Month"
  ringDownRangeDayEntryLabel <- labelNewWithMnemonic "Day"
  ringDownRangeHourEntryLabel <- labelNewWithMnemonic "Hour"
  ringDownRangeMinuteEntryLabel <- labelNewWithMnemonic "Minute"
  ringDownRangeSecondEntryLabel <- labelNewWithMnemonic "Second (JST)"
  ringDownRangeObsTimeEntryLabel <- labelNewWithMnemonic "OBS Time [s]"
  ringDownRangeMassEntryLabel <- labelNewWithMnemonic "mass [M_solar]"
  ringDownRangeKerrParamEntryLabel <- labelNewWithMnemonic "Kerr parameter"
  ringDownRangeMassDefectEntryLabel <- labelNewWithMnemonic "mass defect ratio"
  ringDownRangeIniPhaseEntryLabel <- labelNewWithMnemonic "Initial Phase [rad]"
  ringDownRangeThresholdEntryLabel <- labelNewWithMnemonic "Threshold SNR"

  ringDownRangeYearEntry <- entryNew
  ringDownRangeMonthEntry <- entryNew
  ringDownRangeDayEntry <- entryNew
  ringDownRangeHourEntry <- entryNew
  ringDownRangeMinuteEntry <- entryNew
  ringDownRangeSecondEntry <- entryNew
  ringDownRangeObsTimeEntry <- entryNew
  ringDownRangeMassEntry <- entryNew
  ringDownRangeKerrParamEntry <- entryNew
  ringDownRangeMassDefectEntry <- entryNew
  ringDownRangeIniPhaseEntry <- entryNew
  ringDownRangeThresholdEntry <- entryNew

  ringDownRangeClose <- buttonNewWithLabel "Close"
  ringDownRangeExecute <- buttonNewWithLabel "Execute"

  entrySetText ringDownRangeYearEntry "2013"
  entrySetText ringDownRangeMonthEntry "10"
  entrySetText ringDownRangeDayEntry "21"
  entrySetText ringDownRangeHourEntry "21"
  entrySetText ringDownRangeMinuteEntry "0"
  entrySetText ringDownRangeSecondEntry "0"
  entrySetText ringDownRangeObsTimeEntry "300"
  entrySetText ringDownRangeMassEntry "10.0"
  entrySetText ringDownRangeKerrParamEntry "0.98"
  entrySetText ringDownRangeMassDefectEntry "0.03"
  entrySetText ringDownRangeIniPhaseEntry "0.0"
  entrySetText ringDownRangeThresholdEntry "8.0"
  
  {--  Set Parameters of the objects  --}
  set ringDownRangeWindow [ windowTitle := "RingDown Range",
                      windowDefaultWidth := 200,
                      windowDefaultHeight := 450,
                      containerChild := ringDownRangeVBox,
                      containerBorderWidth := 20 ]

  {--  Arrange object in window  --}
  boxPackStartDefaults ringDownRangeVBox ringDownRangeHBoxYear
  boxPackStartDefaults ringDownRangeHBoxYear ringDownRangeYearEntryLabel
  boxPackStartDefaults ringDownRangeHBoxYear ringDownRangeYearEntry
  boxPackStartDefaults ringDownRangeVBox ringDownRangeHBoxMonth
  boxPackStartDefaults ringDownRangeHBoxMonth ringDownRangeMonthEntryLabel
  boxPackStartDefaults ringDownRangeHBoxMonth ringDownRangeMonthEntry
  boxPackStartDefaults ringDownRangeVBox ringDownRangeHBoxDay
  boxPackStartDefaults ringDownRangeHBoxDay ringDownRangeDayEntryLabel
  boxPackStartDefaults ringDownRangeHBoxDay ringDownRangeDayEntry
  boxPackStartDefaults ringDownRangeVBox ringDownRangeHBoxHour
  boxPackStartDefaults ringDownRangeHBoxHour ringDownRangeHourEntryLabel
  boxPackStartDefaults ringDownRangeHBoxHour ringDownRangeHourEntry
  boxPackStartDefaults ringDownRangeVBox ringDownRangeHBoxMinute
  boxPackStartDefaults ringDownRangeHBoxMinute ringDownRangeMinuteEntryLabel
  boxPackStartDefaults ringDownRangeHBoxMinute ringDownRangeMinuteEntry
  boxPackStartDefaults ringDownRangeVBox ringDownRangeHBoxSecond
  boxPackStartDefaults ringDownRangeHBoxSecond ringDownRangeSecondEntryLabel
  boxPackStartDefaults ringDownRangeHBoxSecond ringDownRangeSecondEntry

  boxPackStartDefaults ringDownRangeVBox ringDownRangeHBoxObsTime
  boxPackStartDefaults ringDownRangeHBoxObsTime ringDownRangeObsTimeEntryLabel
  boxPackStartDefaults ringDownRangeHBoxObsTime ringDownRangeObsTimeEntry
  boxPackStartDefaults ringDownRangeVBox ringDownRangeHBoxMass
  boxPackStartDefaults ringDownRangeHBoxMass ringDownRangeMassEntryLabel
  boxPackStartDefaults ringDownRangeHBoxMass ringDownRangeMassEntry
  boxPackStartDefaults ringDownRangeVBox ringDownRangeHBoxKerrParam
  boxPackStartDefaults ringDownRangeHBoxKerrParam ringDownRangeKerrParamEntryLabel
  boxPackStartDefaults ringDownRangeHBoxKerrParam ringDownRangeKerrParamEntry
  boxPackStartDefaults ringDownRangeVBox ringDownRangeHBoxMassDefect
  boxPackStartDefaults ringDownRangeHBoxMassDefect ringDownRangeMassDefectEntryLabel
  boxPackStartDefaults ringDownRangeHBoxMassDefect ringDownRangeMassDefectEntry
  boxPackStartDefaults ringDownRangeVBox ringDownRangeHBoxIniPhase
  boxPackStartDefaults ringDownRangeHBoxIniPhase ringDownRangeIniPhaseEntryLabel
  boxPackStartDefaults ringDownRangeHBoxIniPhase ringDownRangeIniPhaseEntry
  boxPackStartDefaults ringDownRangeVBox ringDownRangeHBoxThreshold
  boxPackStartDefaults ringDownRangeHBoxThreshold ringDownRangeThresholdEntryLabel
  boxPackStartDefaults ringDownRangeHBoxThreshold ringDownRangeThresholdEntry
  boxPackStartDefaults ringDownRangeVBox ringDownRangeHBoxButtons
  boxPackStartDefaults ringDownRangeHBoxButtons ringDownRangeClose
  boxPackStartDefaults ringDownRangeHBoxButtons ringDownRangeExecute

  {--  Execute  --}
  onClicked ringDownRangeClose $ do
    putStrLn "Closed RingDownRange Window"
    widgetDestroy ringDownRangeWindow
  onClicked ringDownRangeExecute $ do
    putStrLn "Execute"

    s_temp <- entryGetText ringDownRangeYearEntry
    let ringDYear = read s_temp :: Int
    s_temp <- entryGetText ringDownRangeMonthEntry
    let ringDMonth = read s_temp :: Int
    s_temp <- entryGetText ringDownRangeDayEntry
    let ringDDay = read s_temp :: Int
    s_temp <- entryGetText ringDownRangeHourEntry
    let ringDHour = read s_temp :: Int
    s_temp <- entryGetText ringDownRangeMinuteEntry
    let ringDMinute = read s_temp :: Int
    s_temp <- entryGetText ringDownRangeSecondEntry
    let ringDSecond = read s_temp :: Int

    let ringDownRangeDateStr = HGGS.iDate2sDate ringDYear ringDMonth ringDDay ringDHour ringDMinute ringDSecond
    putStrLn ("   JST Time: " ++ ringDownRangeDateStr)
    let hogeGPS = HTG.time2gps ringDownRangeDateStr
    putStrLn ("   GPS Time: " ++ hogeGPS)
    s_temp <- entryGetText ringDownRangeObsTimeEntry
    let ringDObsTime = read s_temp :: Int
    putStrLn ("   Obs Time: " ++ (show ringDObsTime) )
    s_temp <- entryGetText ringDownRangeMassEntry
    let ringDMass = read s_temp :: Double
    putStrLn ("       Mass: " ++ (show ringDMass) )
    s_temp <- entryGetText ringDownRangeKerrParamEntry
    let ringDKerrParam = read s_temp :: Double
    putStrLn (" Kerr Param: " ++ (show ringDKerrParam) )
    s_temp <- entryGetText ringDownRangeMassDefectEntry
    let ringDMassDefect = read s_temp :: Double
    putStrLn ("Mass defect: " ++ (show ringDMassDefect) )
    s_temp <- entryGetText ringDownRangeIniPhaseEntry
    let ringDIniPhase = read s_temp :: Double
    putStrLn ("  Ini Phase: " ++ (show ringDIniPhase) )
    s_temp <- entryGetText ringDownRangeThresholdEntry
    let ringDThreshold = read s_temp :: Double
    putStrLn ("   Thresold: " ++ (show ringDThreshold) )

    {-- detecter data IO and format --}
    detDataStr <- readFile $ HGGS.haskalOpt ++ "/sensitivities/bKAGRA/prebKAGRA.dat"
    let detData = map HGGS.amp2psd $ map HGGS.convert_LtoT2 $ map (map read) $ map words $ lines detDataStr :: [(Double, Double)]
    {-- end of detecter data IO and format --}
    {-- Monitor tool --}
    ringDDist <- CM.forM [1.0*ringDMass, 10.0*ringDMass..1000.0*ringDMass] $ \mass -> 
--      return $ HMRIRD.distRingdown mass ringDThreshold ringDKerrParam ringDMassDefect ringDIniPhase detData
      return $ HMRIRD.distRingdown mass detData
    HPPR.hroot_core [1.0*ringDMass, 10.0*ringDMass..1000.0*ringDMass] ringDDist "mass [M_sol]" "Distance [Mpc]" HPPOR.LogXY HPPOR.Line "X11"
    {-- End of Monitor Tool --}

  {--  Exit Process  --}
  onDestroy ringDownRangeWindow mainQuit
  widgetShowAll ringDownRangeWindow
  mainGUI

