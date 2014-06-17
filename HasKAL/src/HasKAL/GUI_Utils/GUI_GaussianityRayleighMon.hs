{-******************************************
  *     File Name: GUI_GaussianityRayleighMon.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/06/17 23:35:08
  *******************************************-}

module HasKAL.GUI_Utils.GUI_GaussianityRayleighMon(
   hasKalGuiRayleighMon
) where

import Graphics.UI.Gtk
import qualified Control.Monad as CM -- forM
import qualified Data.Maybe as DM -- fromJust
import qualified System.IO.Unsafe as SIOU -- unsafePerformIO

import qualified HasKAL.FrameUtils.FrameUtils as HFF
import qualified HasKAL.FrameUtils.PickUpFileName as HFP
import qualified HasKAL.GUI_Utils.GUI_Supplement as HGGS
import qualified HasKAL.MonitorUtils.RayleighMon.RayleighMon as HMRRM
import qualified HasKAL.PlotUtils.PlotUtils as HPP
import qualified HasKAL.TimeUtils.GPSfunction as HTG

import qualified HasKAL.SpectrumUtils.SpectrumUtils as HSS

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
  rayleighMonHBoxFClust <- hBoxNew True 5
  rayleighMonHBoxButtons <- hBoxNew True 5

  rayleighMonCacheOpener <- HGGS.fileOpenButtonNewWithLabelDefault "Cache file" $ HGGS.haskalOpt ++ "/cachefiles/gwffiles_sorted.lst"
  rayleighMonYearEntry <- HGGS.entryNewWithLabelDefault "Year" "2014"
  rayleighMonMonthEntry <- HGGS.entryNewWithLabelDefault "Month" "3"
  rayleighMonDayEntry <- HGGS.entryNewWithLabelDefault "Day" "17"
  rayleighMonHourEntry <- HGGS.entryNewWithLabelDefault "Hour" "16"
  rayleighMonMinuteEntry <- HGGS.entryNewWithLabelDefault "Minute" "15"
  rayleighMonSecondEntry <- HGGS.entryNewWithLabelDefault "Second (JST)" "12"
  rayleighMonObsTimeEntry <- HGGS.entryNewWithLabelDefault "OBS Time [s]" "128"
  rayleighMonSamplingEntry <- HGGS.entryNewWithLabelDefault "fsample [Hz]" "16384.0"
  rayleighMonStrideEntry <- HGGS.entryNewWithLabelDefault "Stride Num" "65536"
  rayleighMonFClustEntry <- HGGS.entryNewWithLabelDefault "f Clustering Num" "64"

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
  HGGS.boxPackStartDefaultsPair rayleighMonHBoxCache rayleighMonCacheOpener
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
  boxPackStartDefaults rayleighMonVBox rayleighMonHBoxFClust
  HGGS.boxPackStartDefaultsPair rayleighMonHBoxFClust rayleighMonFClustEntry

  boxPackStartDefaults rayleighMonVBox rayleighMonHBoxButtons
  mapM (boxPackStartDefaults rayleighMonHBoxButtons) [rayleighMonClose, rayleighMonExecute]

  {--  Execute --}
  onClicked rayleighMonClose $ do
    putStrLn "Closed RayleighMon Window"
    widgetDestroy rayleighMonWindow
  onClicked rayleighMonExecute $ do
    putStrLn "Execute"
    let rmCache = DM.fromJust $ SIOU.unsafePerformIO $ fileChooserGetFilename $ snd rayleighMonCacheOpener
        rmYear = read $ SIOU.unsafePerformIO $ entryGetText $ snd rayleighMonYearEntry :: Int
        rmMonth = read $ SIOU.unsafePerformIO $ entryGetText $ snd rayleighMonMonthEntry :: Int
        rmDay = read $ SIOU.unsafePerformIO $ entryGetText $ snd rayleighMonDayEntry :: Int
        rmHour = read $ SIOU.unsafePerformIO $ entryGetText $ snd rayleighMonHourEntry :: Int
        rmMinute = read $ SIOU.unsafePerformIO $ entryGetText $ snd rayleighMonMinuteEntry :: Int
        rmSecond = read $ SIOU.unsafePerformIO $ entryGetText $ snd rayleighMonSecondEntry :: Int
        rayleighMonDateStr = HGGS.iDate2sDate rmYear rmMonth rmDay rmHour rmMinute rmSecond
        rmGPS = read $ HTG.time2gps rayleighMonDateStr :: Integer
        rmObsTime = read $ SIOU.unsafePerformIO $ entryGetText $ snd rayleighMonObsTimeEntry :: Integer
        rmSampling = read $ SIOU.unsafePerformIO $ entryGetText $ snd rayleighMonSamplingEntry :: Double
        rmStride = read $ SIOU.unsafePerformIO $ entryGetText $ snd rayleighMonStrideEntry :: Int
        rmFClust = read $ SIOU.unsafePerformIO $ entryGetText $ snd rayleighMonFClustEntry :: Int
        dF = (fromIntegral rmFClust) * rmSampling / (realToFrac rmStride)
    putStrLn ("   JST Time: " ++ rayleighMonDateStr)
    putStrLn ("   GPS Time: " ++ (show rmGPS) )
    putStrLn ("   Obs Time: " ++ (show rmObsTime) )
    putStrLn ("    fsample: " ++ (show rmSampling) )
    putStrLn ("     stride: " ++ (show rmStride) )
{--}
    let snf = getAvePsdFromGPS rmStride rmSampling 32 rmGPS (activeChannelLabels !! 0) rmCache
        frData = getDataFromGPS rmGPS rmObsTime (activeChannelLabels !! 0) rmCache
        quantiles = HMRRM.rayleighMon' rmStride rmFClust rmSampling 0.95 snf frData
    HPP.scatter_plot_2d "RayleighMon 0.95 quantile" "x" 2.0 (720,480) $ zip [0, dF..rmSampling/2] quantiles
{----}

  {--  Exit Process  --}
  onDestroy rayleighMonWindow mainQuit
  widgetShowAll rayleighMonWindow
  mainGUI


{-- Supplementary Functions --}
readFrame' :: String -> String -> IO [Double]
readFrame' = (CM.liftM ((map realToFrac).HFF.eval).).HFF.readFrame

flip231 :: (a -> b -> c -> d) -> b -> c -> a -> d
flip231 = (flip.).flip

getAvePsdFromGPS :: Int -> Double -> Int -> Integer -> String -> String -> [Double]
getAvePsdFromGPS numT fs aveNum gpsD channel cache = map snd.(flip231 HSS.gwpsd numT fs).(take $ aveNum*numT).concat $ datW
  where datW = SIOU.unsafePerformIO $ mapM (readFrame' channel) $ HFP.pickUpFileNameinFile gpsW (gpsD-1) cache
        gpsW = (-) gpsD $ ceiling $ (fromIntegral $ aveNum*numT) / fs

getDataFromGPS :: Integer -> Integer -> String -> String -> [Double]
getDataFromGPS gpsD obsD channel cache = concat $ SIOU.unsafePerformIO $ mapM (readFrame' channel) filelist
  where filelist = HFP.pickUpFileNameinFile gpsD (gpsD+obsD-1) cache
