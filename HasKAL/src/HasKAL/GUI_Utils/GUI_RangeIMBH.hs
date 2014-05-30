{-******************************************
  *     File Name: GUI_RangeIMBH.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/05/30 22:27:16
  *******************************************-}

module HasKAL.GUI_Utils.GUI_RangeIMBH(
   hasKalGuiIMR'Range
) where

import Graphics.UI.Gtk
import qualified Control.Monad as CM -- forM
import qualified System.IO.Unsafe as SIOU -- unsafePerformIO

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

  imrRangeYearEntry <- HGGS.entryNewWithLabelDefault "Year" "2013"
  imrRangeMonthEntry <- HGGS.entryNewWithLabelDefault "Month" "10"
  imrRangeDayEntry <- HGGS.entryNewWithLabelDefault "Day" "21"
  imrRangeHourEntry <- HGGS.entryNewWithLabelDefault "Hour" "21"
  imrRangeMinuteEntry <- HGGS.entryNewWithLabelDefault "Minute" "0"
  imrRangeSecondEntry <- HGGS.entryNewWithLabelDefault "Second (JST)" "0"
  imrRangeObsTimeEntry <- HGGS.entryNewWithLabelDefault "OBS Time [s]" "300"
  imrRangeMass1Entry <- HGGS.entryNewWithLabelDefault "mass_min [M_solar]" "1.0"
  imrRangeMass2Entry <- HGGS.entryNewWithLabelDefault "mass_max [M_solar]" "300.0"

  imrRangeClose <- buttonNewWithLabel "Close"
  imrRangeExecute <- buttonNewWithLabel "Execute"

  {--  Set Parameters of the objects  --}
  set imrRangeWindow [ windowTitle := "Inspiral-Merger-Ringdown Range",
                      windowDefaultWidth := 200,
                      windowDefaultHeight := 450,
                      containerChild := imrRangeVBox,
                      containerBorderWidth := 20 ]

  {--  Arrange object in window  --}
  boxPackStartDefaults imrRangeVBox imrRangeHBoxYear
  HGGS.boxPackStartDefaultsPair imrRangeHBoxYear imrRangeYearEntry
  boxPackStartDefaults imrRangeVBox imrRangeHBoxMonth
  HGGS.boxPackStartDefaultsPair imrRangeHBoxMonth imrRangeMonthEntry
  boxPackStartDefaults imrRangeVBox imrRangeHBoxDay
  HGGS.boxPackStartDefaultsPair imrRangeHBoxDay imrRangeDayEntry
  boxPackStartDefaults imrRangeVBox imrRangeHBoxHour
  HGGS.boxPackStartDefaultsPair imrRangeHBoxHour imrRangeHourEntry
  boxPackStartDefaults imrRangeVBox imrRangeHBoxMinute
  HGGS.boxPackStartDefaultsPair imrRangeHBoxMinute imrRangeMinuteEntry
  boxPackStartDefaults imrRangeVBox imrRangeHBoxSecond
  HGGS.boxPackStartDefaultsPair imrRangeHBoxSecond imrRangeSecondEntry
  boxPackStartDefaults imrRangeVBox imrRangeHBoxObsTime
  HGGS.boxPackStartDefaultsPair imrRangeHBoxObsTime imrRangeObsTimeEntry
  boxPackStartDefaults imrRangeVBox imrRangeHBoxMass1
  HGGS.boxPackStartDefaultsPair imrRangeHBoxMass1 imrRangeMass1Entry
  boxPackStartDefaults imrRangeVBox imrRangeHBoxMass2
  HGGS.boxPackStartDefaultsPair imrRangeHBoxMass2 imrRangeMass2Entry

  boxPackStartDefaults imrRangeVBox imrRangeHBoxButtons
  mapM (boxPackStartDefaults imrRangeHBoxButtons) [imrRangeClose, imrRangeExecute]

  {--  Execute  --}
  onClicked imrRangeClose $ do
    putStrLn "Closed Inspiral-Merger-RingdownRange Window"
    widgetDestroy imrRangeWindow
  onClicked imrRangeExecute $ do
    putStrLn "Execute"

    let imrYear = read $ SIOU.unsafePerformIO $ entryGetText $ snd imrRangeYearEntry :: Int    
        imrMonth = read $ SIOU.unsafePerformIO $ entryGetText $ snd imrRangeMonthEntry :: Int
        imrDay = read $ SIOU.unsafePerformIO $ entryGetText $ snd imrRangeDayEntry :: Int
        imrHour = read $ SIOU.unsafePerformIO $ entryGetText $ snd imrRangeHourEntry :: Int
        imrMinute = read $ SIOU.unsafePerformIO $ entryGetText $ snd imrRangeMinuteEntry :: Int
        imrSecond = read $ SIOU.unsafePerformIO $ entryGetText $ snd imrRangeSecondEntry :: Int
        imrRangeDateStr = HGGS.iDate2sDate imrYear imrMonth imrDay imrHour imrMinute imrSecond
        imrGPS = HTG.time2gps imrRangeDateStr
        imrObsTime = read $ SIOU.unsafePerformIO $ entryGetText $ snd imrRangeObsTimeEntry :: Int
        imrMass1 = read $ SIOU.unsafePerformIO $ entryGetText $ snd imrRangeMass1Entry :: Double
        imrMass2 = read $ SIOU.unsafePerformIO $ entryGetText $ snd imrRangeMass2Entry :: Double
    putStrLn ("   JST Time: " ++ imrRangeDateStr)
    putStrLn ("   GPS Time: " ++ imrGPS)
    putStrLn ("   Obs Time: " ++ (show imrObsTime) )
    putStrLn ("     Mass_1: " ++ (show imrMass1) )
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


