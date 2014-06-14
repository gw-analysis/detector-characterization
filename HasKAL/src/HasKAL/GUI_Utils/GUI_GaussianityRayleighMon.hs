{-******************************************
  *     File Name: GUI_GaussianityRayleighMon.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/06/13 18:16:39
  *******************************************-}

module HasKAL.GUI_Utils.GUI_GaussianityRayleighMon(
   hasKalGuiRayleighMon
) where

import Graphics.UI.Gtk
import qualified Control.Monad as CM -- forM
import qualified System.IO as SIO -- openFile
import qualified System.IO.Unsafe as SIOU -- unsafePerformIO

import qualified HasKAL.FrameUtils.FrameUtils as HFF
import qualified HasKAL.FrameUtils.PickUpFileName as HFP
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
  rayleighMonHBoxCache <- hBoxNew True 5
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

  rayleighMonCacheEntry <- HGGS.entryNewWithLabelDefault "Cache file" "gwffiles_sorted.lst"
  rayleighMonYearEntry <- HGGS.entryNewWithLabelDefault "Year" "2014"
  rayleighMonMonthEntry <- HGGS.entryNewWithLabelDefault "Month" "3"
  rayleighMonDayEntry <- HGGS.entryNewWithLabelDefault "Day" "17"
  rayleighMonHourEntry <- HGGS.entryNewWithLabelDefault "Hour" "16"
  rayleighMonMinuteEntry <- HGGS.entryNewWithLabelDefault "Minute" "15"
  rayleighMonSecondEntry <- HGGS.entryNewWithLabelDefault "Second (JST)" "12"
  rayleighMonObsTimeEntry <- HGGS.entryNewWithLabelDefault "OBS Time [s]" "16"
  rayleighMonSamplingEntry <- HGGS.entryNewWithLabelDefault "fsample [Hz]" "16384.0"
  rayleighMonStrideEntry <- HGGS.entryNewWithLabelDefault "Stride Num" "1024"

  rayleighMonClose <- buttonNewWithLabel "Close"
  rayleighMonExecute <- buttonNewWithLabel "Execute"

  {--  Set Parameters of the objects  --}
  set rayleighMonWindow [ windowTitle := "RayleighMon",
                      windowDefaultWidth := 300,
                      windowDefaultHeight := 450,
                      containerChild := rayleighMonVBox,
                      containerBorderWidth := 20 ]

  {--  Arrange object in window  --}
  boxPackStartDefaults rayleighMonVBox rayleighMonHBoxCache
  HGGS.boxPackStartDefaultsPair rayleighMonHBoxCache rayleighMonCacheEntry
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
    rmCache <- entryGetText $ snd rayleighMonCacheEntry
    let rmYear = read $ SIOU.unsafePerformIO $ entryGetText $ snd rayleighMonYearEntry :: Int
        rmMonth = read $ SIOU.unsafePerformIO $ entryGetText $ snd rayleighMonMonthEntry :: Int
        rmDay = read $ SIOU.unsafePerformIO $ entryGetText $ snd rayleighMonDayEntry :: Int
        rmHour = read $ SIOU.unsafePerformIO $ entryGetText $ snd rayleighMonHourEntry :: Int
        rmMinute = read $ SIOU.unsafePerformIO $ entryGetText $ snd rayleighMonMinuteEntry :: Int
        rmSecond = read $ SIOU.unsafePerformIO $ entryGetText $ snd rayleighMonSecondEntry :: Int
        rayleighMonDateStr = HGGS.iDate2sDate rmYear rmMonth rmDay rmHour rmMinute rmSecond
        rmGPS = read $ HTG.time2gps rayleighMonDateStr :: Int
        rmObsTime = read $ SIOU.unsafePerformIO $ entryGetText $ snd rayleighMonObsTimeEntry :: Int
        rmSampling = read $ SIOU.unsafePerformIO $ entryGetText $ snd rayleighMonSamplingEntry :: Double
        rmStride = read $ SIOU.unsafePerformIO $ entryGetText $ snd rayleighMonStrideEntry :: Int
        rmFiles = HFP.pickUpFileNameinFile (fromIntegral rmGPS) (fromIntegral $ rmGPS+rmObsTime) $ HGGS.haskalOpt ++ "/cachefiles/" ++ rmCache
        df = rmSampling / (realToFrac rmStride)
    putStrLn ("   JST Time: " ++ rayleighMonDateStr)
    putStrLn ("   GPS Time: " ++ (show rmGPS) )
    putStrLn ("   Obs Time: " ++ (show rmObsTime) )
    putStrLn ("    fsample: " ++ (show rmSampling) )
    putStrLn ("     stride: " ++ (show rmStride) )
{--}
    frData <- CM.liftM concat $ mapM (readFrame' $ activeChannelLabels !! 0) rmFiles
--    frData <- HFF.readFrame (activeChannelLabels !! 0) $ HGGS.haskalOpt ++ "/sample-data/test-1066392016-300.gwf" -- 複数チャンネルに対応させる
    HPPR.hroot_core ([0,df..(rmSampling / 2.0)-df]) (HMRRM.rayleighMon rmStride 1 rmSampling 0.9 frData) "frequency [Hz]" "noise level [/rHz]" HPPOR.LogXY HPPOR.Line "X11"
--    HPPR.hroot_core (map fromIntegral [0,1..(rmStride `div` 2 {-+ 1-})]) (HMRRM.rayleighMon rmStride 1 0.9 (map realToFrac (HFF.eval frData))) "frequency [Hz]" "noise level [/rHz]" HPPOR.LogXY HPPOR.Line "X11"
    -- 横軸の値を直す(1秒スペクトルなので今は正しい)
{----}

  {--  Exit Process  --}
  onDestroy rayleighMonWindow mainQuit
  widgetShowAll rayleighMonWindow
  mainGUI


{-- Supplementary Functions --}
readFrame' :: String -> String -> IO [Double]
readFrame' channel_Name framefile_Name = CM.liftM ((map realToFrac).HFF.eval) $ HFF.readFrame channel_Name framefile_Name

