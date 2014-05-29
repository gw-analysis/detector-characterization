{-******************************************
  *     File Name: GUI_GlitchKleineWelle.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/05/29 11:50:49
  *******************************************-}

module HasKAL.GUI_Utils.GUI_GlitchKleineWelle(
   hasKalGuiKleineWelle
) where

import Graphics.UI.Gtk
import qualified Control.Monad as CM -- forM

import qualified HasKAL.FrameUtils.PickUpFileName as HFP
import qualified HasKAL.GUI_Utils.GUI_Supplement as HGGS
import qualified HasKAL.MonitorUtils.KleineWelle.EXTKleineWelle as HMKKW
import qualified HasKAL.PlotUtils.PlotUtils as HPP
import qualified HasKAL.TimeUtils.GPSfunction as HTG

{- KleineWelle Window
-- test code
main = IO ()
main = hasKalGuiKleineWelle "Channel_Name"
-- arguments: channel_name
-}
hasKalGuiKleineWelle :: [String] -> IO ()
hasKalGuiKleineWelle kleineWelleActiveLabels = do
  initGUI
  putStrLn "Open KleineWelle Window"

  {--  Fixed value for KleineWelle  --}
  let kwBasename = "KW_"
  let kwTransientDuration = 4.0
  let kwDecimateFactor = -1
  let kwUnowen_2 = 2
  let kwOptFilePref = "optKW_"
--  let kwListFile = "gwffilelist.txt"

  {--  for lwtprint  --}
  let kwChannelLabels = [ "ifo", "peak_time", "peak_time_ns", "start_time", "start_time_ns", "duration", "search", "central_freq", "channel", "amplitude", "snr", "confidence", "chisq", "chisq_dof", "bandwidth", "event_id", "process_id", "table" ]
  kwChannelScroll <- scrolledWindowNew Nothing Nothing
  kwChannelBBox <- vButtonBoxNew
  kwChannelCButtons <- mapM checkButtonNewWithLabel kwChannelLabels

  {--  Information  --}
  let kwNumOfChannel = length kwChannelLabels

  {--  Create new object --}
  kleineWelleWindow <- windowNew
--  kleineWelleVBox <- vBoxNew True 5
  kleineWelleVBox2 <- vBoxNew True 5
  kleineWelleHBoxCache <- hBoxNew True 5
  kleineWelleHBoxScroll <- hBoxNew True 5
  kleineWelleHBoxDateL <- hBoxNew True 5
  kleineWelleHBoxDateE <- hBoxNew True 5
  kleineWelleHBoxTimeL <- hBoxNew True 5
  kleineWelleHBoxTimeE <- hBoxNew True 5
  kleineWelleHBoxObs <- hBoxNew True 5
  kleineWelleHBoxStride <- hBoxNew True 5
  kleineWelleHBoxSignificance <- hBoxNew True 5
  kleineWelleHBoxThreshold <- hBoxNew True 5
  kleineWelleHBoxLowCutOff <- hBoxNew True 5
  kleineWelleHBoxHighCutOff <- hBoxNew True 5
  kleineWelleHBoxExecute <- hBoxNew True 5

  kleineWelleChannelScroll <- scrolledWindowNew Nothing Nothing
  kleineWelleChannelBBox <- vButtonBoxNew

  kleineWelleCacheEntryLable <- labelNewWithMnemonic "Cache file"
  kleineWelleYearEntryLable <- labelNewWithMnemonic "Year"
  kleineWelleMonthEntryLable <- labelNewWithMnemonic "Month"
  kleineWelleDayEntryLable <- labelNewWithMnemonic "Day"
  kleineWelleHourEntryLable <- labelNewWithMnemonic "Hour"
  kleineWelleMinuteEntryLable <- labelNewWithMnemonic "Minute"
  kleineWelleSecondEntryLable <- labelNewWithMnemonic "Second (JST)"

  kleineWelleObsEntryLable <- labelNewWithMnemonic "OBS Time [s]"
  kleineWelleStrideEntryLable <- labelNewWithMnemonic "Stride"
  kleineWelleSignificanceEntryLable <- labelNewWithMnemonic "Significance"
  kleineWelleThresholdEntryLable <- labelNewWithMnemonic "Threshold"
  kleineWelleLowCutOffEntryLable <- labelNewWithMnemonic "LowCutOff [Hz]"
  kleineWelleHighCutOffEntryLable <- labelNewWithMnemonic "HighCutOff [Hz]"
  kleineWelleGpsEntry <- entryNew

  kleineWelleCacheEntry <- entryNew
  kleineWelleYearEntry <- entryNew
  kleineWelleMonthEntry <- entryNew
  kleineWelleDayEntry <- entryNew
  kleineWelleHourEntry <- entryNew
  kleineWelleMinuteEntry <- entryNew
  kleineWelleSecondEntry <- entryNew

  kleineWelleObsEntry <- entryNew
  kleineWelleStrideEntry <- entryNew
  kleineWelleSignificanceEntry <- entryNew
  kleineWelleThresholdEntry <- entryNew
  kleineWelleLowCutOffEntry <- entryNew
  kleineWelleHighCutOffEntry <- entryNew
  kleineWelleClose <- buttonNewWithLabel "Close"
  kleineWelleExecute <- buttonNewWithLabel "Execute"

  entrySetText kleineWelleCacheEntry "gwffiles_sorted.lst"
  entrySetText kleineWelleYearEntry "2014"
  entrySetText kleineWelleMonthEntry "3"
  entrySetText kleineWelleDayEntry "17"
  entrySetText kleineWelleHourEntry "16"
  entrySetText kleineWelleMinuteEntry "15"
  entrySetText kleineWelleSecondEntry "12"

  entrySetText kleineWelleObsEntry "128"
  entrySetText kleineWelleStrideEntry "16"
  entrySetText kleineWelleSignificanceEntry "2.0"
  entrySetText kleineWelleThresholdEntry "3.0"
  entrySetText kleineWelleLowCutOffEntry "10"
  entrySetText kleineWelleHighCutOffEntry "1000"

  {--  Set Parameters of the objects  --}
  set kleineWelleWindow [ windowTitle := "KleineWelle",
                      windowDefaultWidth := 200,
                      windowDefaultHeight := 450,
                      containerChild := kleineWelleHBoxScroll,
                      containerBorderWidth := 20 ]
  scrolledWindowSetPolicy kleineWelleChannelScroll PolicyAutomatic PolicyAutomatic


  {--  Arrange object in window  --}
  mapM (boxPackStartDefaults kwChannelBBox) kwChannelCButtons
  scrolledWindowAddWithViewport kwChannelScroll kwChannelBBox
  boxPackStartDefaults kleineWelleHBoxScroll kwChannelScroll

  boxPackStartDefaults kleineWelleHBoxScroll kleineWelleVBox2
  boxPackStartDefaults kleineWelleVBox2 kleineWelleHBoxCache
  boxPackStartDefaults kleineWelleHBoxCache kleineWelleCacheEntryLable
  boxPackStartDefaults kleineWelleHBoxCache kleineWelleCacheEntry

  boxPackStartDefaults kleineWelleVBox2 kleineWelleHBoxDateL
  boxPackStartDefaults kleineWelleHBoxDateL kleineWelleYearEntryLable
  boxPackStartDefaults kleineWelleHBoxDateL kleineWelleMonthEntryLable
  boxPackStartDefaults kleineWelleHBoxDateL kleineWelleDayEntryLable
  boxPackStartDefaults kleineWelleVBox2 kleineWelleHBoxDateE
  boxPackStartDefaults kleineWelleHBoxDateE kleineWelleYearEntry
  boxPackStartDefaults kleineWelleHBoxDateE kleineWelleMonthEntry
  boxPackStartDefaults kleineWelleHBoxDateE kleineWelleDayEntry
  boxPackStartDefaults kleineWelleVBox2 kleineWelleHBoxTimeL
  boxPackStartDefaults kleineWelleHBoxTimeL kleineWelleHourEntryLable
  boxPackStartDefaults kleineWelleHBoxTimeL kleineWelleMinuteEntryLable
  boxPackStartDefaults kleineWelleHBoxTimeL kleineWelleSecondEntryLable
  boxPackStartDefaults kleineWelleVBox2 kleineWelleHBoxTimeE
  boxPackStartDefaults kleineWelleHBoxTimeE kleineWelleHourEntry
  boxPackStartDefaults kleineWelleHBoxTimeE kleineWelleMinuteEntry
  boxPackStartDefaults kleineWelleHBoxTimeE kleineWelleSecondEntry

  boxPackStartDefaults kleineWelleVBox2 kleineWelleHBoxObs
  boxPackStartDefaults kleineWelleHBoxObs kleineWelleObsEntryLable
  boxPackStartDefaults kleineWelleHBoxObs kleineWelleObsEntry
  boxPackStartDefaults kleineWelleVBox2 kleineWelleHBoxStride
  boxPackStartDefaults kleineWelleHBoxStride kleineWelleStrideEntryLable
  boxPackStartDefaults kleineWelleHBoxStride kleineWelleStrideEntry
  boxPackStartDefaults kleineWelleVBox2 kleineWelleHBoxSignificance
  boxPackStartDefaults kleineWelleHBoxSignificance kleineWelleSignificanceEntryLable
  boxPackStartDefaults kleineWelleHBoxSignificance kleineWelleSignificanceEntry
  boxPackStartDefaults kleineWelleVBox2 kleineWelleHBoxThreshold
  boxPackStartDefaults kleineWelleHBoxThreshold kleineWelleThresholdEntryLable
  boxPackStartDefaults kleineWelleHBoxThreshold kleineWelleThresholdEntry
  boxPackStartDefaults kleineWelleVBox2 kleineWelleHBoxLowCutOff
  boxPackStartDefaults kleineWelleHBoxLowCutOff kleineWelleLowCutOffEntryLable
  boxPackStartDefaults kleineWelleHBoxLowCutOff kleineWelleLowCutOffEntry
  boxPackStartDefaults kleineWelleVBox2 kleineWelleHBoxHighCutOff
  boxPackStartDefaults kleineWelleHBoxHighCutOff kleineWelleHighCutOffEntryLable
  boxPackStartDefaults kleineWelleHBoxHighCutOff kleineWelleHighCutOffEntry

  boxPackStartDefaults kleineWelleVBox2 kleineWelleHBoxExecute
  boxPackStartDefaults kleineWelleHBoxExecute kleineWelleClose
  boxPackStartDefaults kleineWelleHBoxExecute kleineWelleExecute


  {--  Execute --}
  onClicked kleineWelleExecute $ do
    putStrLn "Execute"
    let kwActiveLabels = HGGS.getActiveLabels kwChannelCButtons

    kwCache <- entryGetText kleineWelleCacheEntry
    s_temp <- entryGetText kleineWelleYearEntry
    let kwYear = read s_temp :: Int
    s_temp <- entryGetText kleineWelleMonthEntry
    let kwMonth = read s_temp :: Int
    s_temp <- entryGetText kleineWelleDayEntry
    let kwDay = read s_temp :: Int
    s_temp <- entryGetText kleineWelleHourEntry
    let kwHour = read s_temp :: Int
    s_temp <- entryGetText kleineWelleMinuteEntry
    let kwMinute = read s_temp :: Int
    s_temp <- entryGetText kleineWelleSecondEntry
    let kwSecond = read s_temp :: Int
    let kleineWelleDateStr = HGGS.iDate2sDate kwYear kwMonth kwDay kwHour kwMinute kwSecond
    putStrLn ("   JST Time: " ++ kleineWelleDateStr)
    let kwGpsTime = read $ HTG.time2gps kleineWelleDateStr :: Int
    putStrLn ("   GPS Time: " ++ (show kwGpsTime))

    s_temp <- entryGetText kleineWelleObsEntry
    let kwObsTime = read s_temp :: Integer
    putStrLn ("   Obs Time: " ++ (show kwObsTime) )
    s_temp <- entryGetText kleineWelleStrideEntry
    let kwStride = read s_temp :: Int
    putStrLn ("   Stride: " ++ (show kwStride) )
    s_temp <- entryGetText kleineWelleSignificanceEntry
    let kwSignificance = read s_temp :: Double
    putStrLn ("   Significance: " ++ (show kwSignificance) )
    s_temp <- entryGetText kleineWelleThresholdEntry
    let kwThreshold = read s_temp :: Double
    putStrLn ("   Threshold: " ++ (show kwThreshold) )
    s_temp <- entryGetText kleineWelleLowCutOffEntry
    let kwLowCutOff = read s_temp :: Int
    putStrLn ("   LowCutOff: " ++ (show kwLowCutOff) )
    s_temp <- entryGetText kleineWelleHighCutOffEntry
    let kwHighCutOff = read s_temp :: Int
    putStrLn ("   HighCutOff: " ++ (show kwHighCutOff) )
    putStrLn "   Channels: "
    mapM_ putStrLn kleineWelleActiveLabels
    putStrLn "   Column: "
    mapM_ putStrLn kwActiveLabels
{--}
    let kwCasheFile = HGGS.haskalOpt ++ "/cachefiles/" ++ kwCache
    putStrLn $ (show (fromIntegral kwGpsTime)) ++ " ~ " ++ (show (fromIntegral kwGpsTime + kwObsTime))
    HFP.pickUpFileNameinoutFile (fromIntegral kwGpsTime) (fromIntegral kwGpsTime + kwObsTime) kwCasheFile
    let kwListFile = "tmpCachedFrameFile.lst"
{----}
    putStrLn "Generate optM file for KleineWelle"
    lwtOutput <- HMKKW.execKleineWelle kwStride kwBasename kwTransientDuration kwSignificance kwThreshold kwDecimateFactor kleineWelleActiveLabels kwLowCutOff kwHighCutOff kwUnowen_2 kwOptFilePref kwListFile kwGpsTime kwActiveLabels
    let lwtColmunNum = length kwActiveLabels
    putStrLn "Run Plot tool"
    case lwtColmunNum of 2 -> CM.forM lwtOutput $ \lambda -> HPP.scatter_plot_2d "TITLE 2" "HOGEHOGE" 10.0 (640,480) (HGGS.convert_StoDT2L lambda)
                         3 -> CM.forM lwtOutput $ \lambda -> HPP.scatter_plot_3d "TITLE 3" "HOGEHOGE" 10.0 (640,480) (HGGS.convert_StoDT3L lambda)
                         _ -> mapM putStrLn ["Required 2 or 3 columns"]
    return ()
  onClicked kleineWelleClose $ do
    putStrLn "Closed KleineWelle Monitor"
    widgetDestroy kleineWelleWindow

  {--  Exit Process  --}
  onDestroy kleineWelleWindow mainQuit
  widgetShowAll kleineWelleWindow
  mainGUI


