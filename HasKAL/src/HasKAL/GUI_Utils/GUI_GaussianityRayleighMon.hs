{-******************************************
  *     File Name: GUI_GaussianityRayleighMon.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/05/22 12:15:14
  *******************************************-}

module HasKAL.GUI_Utils.GUI_GaussianityRayleighMon(
   hasKalGuiRayleighMon
) where

import Graphics.UI.Gtk
import qualified Control.Monad as CM -- forM
import qualified System.IO as SIO -- openFile

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


  rayleighMonYearEntryLable <- labelNewWithMnemonic "Year"
  rayleighMonMonthEntryLable <- labelNewWithMnemonic "Month"
  rayleighMonDayEntryLable <- labelNewWithMnemonic "Day"
  rayleighMonHourEntryLable <- labelNewWithMnemonic "Hour"
  rayleighMonMinuteEntryLable <- labelNewWithMnemonic "Minute"
  rayleighMonSecondEntryLable <- labelNewWithMnemonic "Second (JST)"
  rayleighMonObsTimeEntryLable <- labelNewWithMnemonic "OBS Time [s]"
  rayleighMonSamplingEntryLable <- labelNewWithMnemonic "fsample [Hz]"
  rayleighMonStrideEntryLable <- labelNewWithMnemonic "Stride Num"

  rayleighMonYearEntry <- entryNew
  rayleighMonMonthEntry <- entryNew
  rayleighMonDayEntry <- entryNew
  rayleighMonHourEntry <- entryNew
  rayleighMonMinuteEntry <- entryNew
  rayleighMonSecondEntry <- entryNew
  rayleighMonObsTimeEntry <- entryNew
  rayleighMonSamplingEntry <- entryNew
  rayleighMonStrideEntry <- entryNew

  rayleighMonClose <- buttonNewWithLabel "Close"
  rayleighMonExecute <- buttonNewWithLabel "Execute"

  entrySetText rayleighMonYearEntry "2013"
  entrySetText rayleighMonMonthEntry "10"
  entrySetText rayleighMonDayEntry "21"
  entrySetText rayleighMonHourEntry "21"
  entrySetText rayleighMonMinuteEntry "0"
  entrySetText rayleighMonSecondEntry "0"
  entrySetText rayleighMonObsTimeEntry "300"
  entrySetText rayleighMonSamplingEntry "1000.0"
  entrySetText rayleighMonStrideEntry "1000"

  {--  Set Parameters of the objects  --}
  set rayleighMonWindow [ windowTitle := "RayleighMon",
                      windowDefaultWidth := 200,
                      windowDefaultHeight := 450,
                      containerChild := rayleighMonVBox,
                      containerBorderWidth := 20 ]


  {--  Arrange object in window  --}
  boxPackStartDefaults rayleighMonVBox rayleighMonHBoxYear
  boxPackStartDefaults rayleighMonHBoxYear rayleighMonYearEntryLable
  boxPackStartDefaults rayleighMonHBoxYear rayleighMonYearEntry
  boxPackStartDefaults rayleighMonVBox rayleighMonHBoxMonth
  boxPackStartDefaults rayleighMonHBoxMonth rayleighMonMonthEntryLable
  boxPackStartDefaults rayleighMonHBoxMonth rayleighMonMonthEntry
  boxPackStartDefaults rayleighMonVBox rayleighMonHBoxDay
  boxPackStartDefaults rayleighMonHBoxDay rayleighMonDayEntryLable
  boxPackStartDefaults rayleighMonHBoxDay rayleighMonDayEntry
  boxPackStartDefaults rayleighMonVBox rayleighMonHBoxHour
  boxPackStartDefaults rayleighMonHBoxHour rayleighMonHourEntryLable
  boxPackStartDefaults rayleighMonHBoxHour rayleighMonHourEntry
  boxPackStartDefaults rayleighMonVBox rayleighMonHBoxMinute
  boxPackStartDefaults rayleighMonHBoxMinute rayleighMonMinuteEntryLable
  boxPackStartDefaults rayleighMonHBoxMinute rayleighMonMinuteEntry
  boxPackStartDefaults rayleighMonVBox rayleighMonHBoxSecond
  boxPackStartDefaults rayleighMonHBoxSecond rayleighMonSecondEntryLable
  boxPackStartDefaults rayleighMonHBoxSecond rayleighMonSecondEntry

  boxPackStartDefaults rayleighMonVBox rayleighMonHBoxObsTime
  boxPackStartDefaults rayleighMonHBoxObsTime rayleighMonObsTimeEntryLable
  boxPackStartDefaults rayleighMonHBoxObsTime rayleighMonObsTimeEntry
  boxPackStartDefaults rayleighMonVBox rayleighMonHBoxSampling
  boxPackStartDefaults rayleighMonHBoxSampling rayleighMonSamplingEntryLable
  boxPackStartDefaults rayleighMonHBoxSampling rayleighMonSamplingEntry
  boxPackStartDefaults rayleighMonVBox rayleighMonHBoxStride
  boxPackStartDefaults rayleighMonHBoxStride rayleighMonStrideEntryLable
  boxPackStartDefaults rayleighMonHBoxStride rayleighMonStrideEntry

  boxPackStartDefaults rayleighMonVBox rayleighMonHBoxButtons
  boxPackStartDefaults rayleighMonHBoxButtons rayleighMonClose
  boxPackStartDefaults rayleighMonHBoxButtons rayleighMonExecute

  {--  Execute --}
  onClicked rayleighMonClose $ do
    putStrLn "Closed RayleighMon Window"
    widgetDestroy rayleighMonWindow
  onClicked rayleighMonExecute $ do
    putStrLn "Execute"

    s_temp <- entryGetText rayleighMonYearEntry
    let rmYear = read s_temp :: Int
    s_temp <- entryGetText rayleighMonMonthEntry
    let rmMonth = (read s_temp :: Int)
    s_temp <- entryGetText rayleighMonDayEntry
    let rmDay = read s_temp :: Int
    s_temp <- entryGetText rayleighMonHourEntry
    let rmHour = read s_temp :: Int
    s_temp <- entryGetText rayleighMonMinuteEntry
    let rmMinute = read s_temp :: Int
    s_temp <- entryGetText rayleighMonSecondEntry
    let rmSecond = read s_temp :: Int

    let rayleighMonDateStr = HGGS.iDate2sDate rmYear rmMonth rmDay rmHour rmMinute rmSecond
    putStrLn ("   JST Time: " ++ rayleighMonDateStr)
    let hogeGPS = HTG.time2gps rayleighMonDateStr
    putStrLn ("   GPS Time: " ++ hogeGPS)
    s_temp <- entryGetText rayleighMonObsTimeEntry
    let rmObsTime = read s_temp :: Int
    putStrLn ("   Obs Time: " ++ (show rmObsTime) )
    s_temp <- entryGetText rayleighMonSamplingEntry
    let rmSampling = read s_temp :: Double
    putStrLn ("    fsample: " ++ (show rmSampling) )
    s_temp <- entryGetText rayleighMonStrideEntry
    let rmStride = read s_temp :: Int
    putStrLn ("     stride: " ++ (show rmStride) )

    frData <- HFF.readFrame (activeChannelLabels !! 0) "../sample-data/test-1066392016-300.gwf" -- 複数チャンネルに対応させる
    HPPR.hroot_core (map fromIntegral [0,1..(rmStride `div` 2 {-+ 1-})]) ( (HGGS.transposed $ HMRRM.rayleighMon rmStride rmStride rmSampling (map realToFrac (HFF.eval frData))) !! 0) "frequency [Hz]" "noise level [/rHz]" HPPOR.LogXY HPPOR.Line "X11"
    -- 横軸の値を直す(1秒スペクトルなので今は正しい)

    {-- 暫定的なファイル出力 --}
    oFile <- SIO.openFile "./GUI-RayleighMon_Result.txt" SIO.WriteMode
    SIO.hPutStrLn oFile (HGGS.convert_DLL2S $ HMRRM.rayleighMon rmStride rmStride rmSampling (map realToFrac (HFF.eval frData)) )
    SIO.hClose oFile
    {--  ここまで、ファイル出力  --}
    --widgetDestroy rayleighMonWindow


  {--  Exit Process  --}
  onDestroy rayleighMonWindow mainQuit
  widgetShowAll rayleighMonWindow
  mainGUI


