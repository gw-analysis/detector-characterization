{-******************************************
  *     File Name: GUI_GaussianityRayleighMon.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/05/30 22:33:31
  *******************************************-}

module HasKAL.GUI_Utils.GUI_GaussianityRayleighMon(
   hasKalGuiRayleighMon
) where

import Graphics.UI.Gtk
import qualified Control.Monad as CM -- forM
import qualified System.IO as SIO -- openFile
import qualified System.IO.Unsafe as SIOU -- unsafePerformIO

import qualified HasKAL.FrameUtils.FrameUtils as HFF
import qualified HasKAL.GUI_Utils.GUI_Supplement as HGGS
import qualified HasKAL.MonitorUtils.RayleighMon.RayleighMon as HMRRM
import qualified HasKAL.PlotUtils.PlotOption.PlotOptionHROOT as HPPOR
import qualified HasKAL.PlotUtils.PlotUtilsHROOT as HPPR
import qualified HasKAL.TimeUtils.GPSfunction as HTG

{-- RayleighMon Window 
-- test code
main = IO ()
main = hasKalGuiRayleighMon ["Channel_Name"]
-- arguments: channel_name
--}
hasKalGuiRayleighMon :: [String] -> IO ()
hasKalGuiRayleighMon activeChannelLabels = do
  initGUI  
  putStrLn "Open RayLeigMon Window"

  {--  Create new object --}
  rayleighMonWindow <- windowNew
  rayleighMonVBox <- vBoxNew True 5
  rayleighMonHBoxYear <- hBoxNew True 5
  rayleighMonHBoxMonth <- hBoxNew True 5
  rayleighMonHBoxDay <- hBoxNew True 5
  rayleighMonHBoxHour <- hBoxNew True 5
  rayleighMonHBoxMinute <- hBoxNew True 5
  rayleighMonHBoxSecond <- hBoxNew True 5
  rayleighMonHBoxObsTime <- hBoxNew True 5
  rayleighMonHBoxSampling <- hBoxNew True 5
  rayleighMonHBoxStride <- hBoxNew True 5
  rayleighMonHBoxButtons <- hBoxNew True 5

  rayleighMonYearEntry <- HGGS.entryNewWithLabelDefault "Year" "2013"
  rayleighMonMonthEntry <- HGGS.entryNewWithLabelDefault "Month" "10"
  rayleighMonDayEntry <- HGGS.entryNewWithLabelDefault "Day" "21"
  rayleighMonHourEntry <- HGGS.entryNewWithLabelDefault "Hour" "21"
  rayleighMonMinuteEntry <- HGGS.entryNewWithLabelDefault "Minute" "0"
  rayleighMonSecondEntry <- HGGS.entryNewWithLabelDefault "Second (JST)" "0"
  rayleighMonObsTimeEntry <- HGGS.entryNewWithLabelDefault "OBS Time [s]" "300"
  rayleighMonSamplingEntry <- HGGS.entryNewWithLabelDefault "fsample [Hz]" "1000.0"
  rayleighMonStrideEntry <- HGGS.entryNewWithLabelDefault "Stride Num" "1000"

  rayleighMonClose <- buttonNewWithLabel "Close"
  rayleighMonExecute <- buttonNewWithLabel "Execute"

  {--  Set Parameters of the objects  --}
  set rayleighMonWindow [ windowTitle := "RayleighMon",
                      windowDefaultWidth := 200,
                      windowDefaultHeight := 450,
                      containerChild := rayleighMonVBox,
                      containerBorderWidth := 20 ]

  {--  Arrange object in window  --}
  boxPackStartDefaults rayleighMonVBox rayleighMonHBoxYear
  HGGS.boxPackStartDefaultsPair rayleighMonHBoxYear rayleighMonYearEntry
  boxPackStartDefaults rayleighMonVBox rayleighMonHBoxMonth
  HGGS.boxPackStartDefaultsPair rayleighMonHBoxMonth rayleighMonMonthEntry
  boxPackStartDefaults rayleighMonVBox rayleighMonHBoxDay
  HGGS.boxPackStartDefaultsPair rayleighMonHBoxDay rayleighMonDayEntry
  boxPackStartDefaults rayleighMonVBox rayleighMonHBoxHour
  HGGS.boxPackStartDefaultsPair rayleighMonHBoxHour rayleighMonHourEntry
  boxPackStartDefaults rayleighMonVBox rayleighMonHBoxMinute
  HGGS.boxPackStartDefaultsPair rayleighMonHBoxMinute rayleighMonMinuteEntry
  boxPackStartDefaults rayleighMonVBox rayleighMonHBoxSecond
  HGGS.boxPackStartDefaultsPair rayleighMonHBoxSecond rayleighMonSecondEntry
  boxPackStartDefaults rayleighMonVBox rayleighMonHBoxObsTime
  HGGS.boxPackStartDefaultsPair rayleighMonHBoxObsTime rayleighMonObsTimeEntry
  boxPackStartDefaults rayleighMonVBox rayleighMonHBoxSampling
  HGGS.boxPackStartDefaultsPair rayleighMonHBoxSampling rayleighMonSamplingEntry
  boxPackStartDefaults rayleighMonVBox rayleighMonHBoxStride
  HGGS.boxPackStartDefaultsPair rayleighMonHBoxStride rayleighMonStrideEntry

  boxPackStartDefaults rayleighMonVBox rayleighMonHBoxButtons
  mapM (boxPackStartDefaults rayleighMonHBoxButtons) [rayleighMonClose, rayleighMonExecute]

  {--  Execute --}
  onClicked rayleighMonClose $ do
    putStrLn "Closed RayleighMon Window"
    widgetDestroy rayleighMonWindow
  onClicked rayleighMonExecute $ do
    putStrLn "Execute"
    let rmYear = read $ SIOU.unsafePerformIO $ entryGetText $ snd rayleighMonYearEntry :: Int
        rmMonth = read $ SIOU.unsafePerformIO $ entryGetText $ snd rayleighMonMonthEntry :: Int
        rmDay = read $ SIOU.unsafePerformIO $ entryGetText $ snd rayleighMonDayEntry :: Int
        rmHour = read $ SIOU.unsafePerformIO $ entryGetText $ snd rayleighMonHourEntry :: Int
        rmMinute = read $ SIOU.unsafePerformIO $ entryGetText $ snd rayleighMonMinuteEntry :: Int
        rmSecond = read $ SIOU.unsafePerformIO $ entryGetText $ snd rayleighMonSecondEntry :: Int
        rayleighMonDateStr = HGGS.iDate2sDate rmYear rmMonth rmDay rmHour rmMinute rmSecond
        rmGPS = HTG.time2gps rayleighMonDateStr
        rmObsTime = read $ SIOU.unsafePerformIO $ entryGetText $ snd rayleighMonObsTimeEntry :: Int
        rmSampling = read $ SIOU.unsafePerformIO $ entryGetText $ snd rayleighMonSamplingEntry :: Double
        rmStride = read $ SIOU.unsafePerformIO $ entryGetText $ snd rayleighMonStrideEntry :: Int
    putStrLn ("   JST Time: " ++ rayleighMonDateStr)
    putStrLn ("   GPS Time: " ++ rmGPS)
    putStrLn ("   Obs Time: " ++ (show rmObsTime) )
    putStrLn ("    fsample: " ++ (show rmSampling) )
    putStrLn ("     stride: " ++ (show rmStride) )

    frData <- HFF.readFrame (activeChannelLabels !! 0) $ HGGS.haskalOpt ++ "/sample-data/test-1066392016-300.gwf" -- 複数チャンネルに対応させる
    HPPR.hroot_core (map fromIntegral [0,1..(rmStride `div` 2 {-+ 1-})]) (HMRRM.rayleighMon rmStride 1 0.9 (map realToFrac (HFF.eval frData))) "frequency [Hz]" "noise level [/rHz]" HPPOR.LogXY HPPOR.Line "X11"
    -- 横軸の値を直す(1秒スペクトルなので今は正しい)

    {-- 暫定的なファイル出力 --}
    oFile <- SIO.openFile "./GUI-RayleighMon_Result.txt" SIO.WriteMode
    SIO.hPutStrLn oFile (HGGS.convert_DL2S $ HMRRM.rayleighMon rmStride 1 0.9 (map realToFrac (HFF.eval frData)) )
    SIO.hClose oFile
    {--  ここまで、ファイル出力  --}
    --widgetDestroy rayleighMonWindow

  {--  Exit Process  --}
  onDestroy rayleighMonWindow mainQuit
  widgetShowAll rayleighMonWindow
  mainGUI


