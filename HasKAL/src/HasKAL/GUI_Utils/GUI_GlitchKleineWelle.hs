{-******************************************
  *     File Name: GUI_GlitchKleineWelle.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/05/22 12:26:28
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
  kleineWelleVBox <- vBoxNew True 5
  kleineWelleVBox2 <- vBoxNew True 5
  kleineWelleHBox0 <- hBoxNew True 5
  kleineWelleHBox <- hBoxNew True 5
  kleineWelleHBox00 <- hBoxNew True 5
  kleineWelleHBox1 <- hBoxNew True 5
  kleineWelleHBox11 <- hBoxNew True 5
  kleineWelleHBox12 <- hBoxNew True 5
  kleineWelleHBox13 <- hBoxNew True 5
  kleineWelleHBox2 <- hBoxNew True 5
  kleineWelleHBox3 <- hBoxNew True 5
  kleineWelleHBox4 <- hBoxNew True 5
  kleineWelleHBox5 <- hBoxNew True 5
  kleineWelleHBox6 <- hBoxNew True 5
  kleineWelleHBox7 <- hBoxNew True 5

  kleineWelleChannelScroll <- scrolledWindowNew Nothing Nothing
  kleineWelleChannelBBox <- vButtonBoxNew
  --kleineWelleChannelCButtons <- mapM checkButtonNewWithLabel kleineWelleChannelLabels
  --kleineWelleGpsEntryLable <- labelNewWithMnemonic "GPS Time [s]"

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
--  entrySetText kleineWelleGpsEntry "1066392016"

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
                      containerChild := kleineWelleHBox00,
                      containerBorderWidth := 20 ]
  scrolledWindowSetPolicy kleineWelleChannelScroll PolicyAutomatic PolicyAutomatic


  {--  Arrange object in window  --}
  mapM (boxPackStartDefaults kwChannelBBox) kwChannelCButtons
  scrolledWindowAddWithViewport kwChannelScroll kwChannelBBox
  boxPackStartDefaults kleineWelleHBox00 kwChannelScroll

  boxPackStartDefaults kleineWelleHBox00 kleineWelleVBox2
  boxPackStartDefaults kleineWelleVBox2 kleineWelleHBox

  boxPackStartDefaults kleineWelleVBox2 kleineWelleHBox1
  boxPackStartDefaults kleineWelleHBox1 kleineWelleYearEntryLable
  boxPackStartDefaults kleineWelleHBox1 kleineWelleMonthEntryLable
  boxPackStartDefaults kleineWelleHBox1 kleineWelleDayEntryLable
  boxPackStartDefaults kleineWelleVBox2 kleineWelleHBox11
  boxPackStartDefaults kleineWelleHBox11 kleineWelleYearEntry
  boxPackStartDefaults kleineWelleHBox11 kleineWelleMonthEntry
  boxPackStartDefaults kleineWelleHBox11 kleineWelleDayEntry
  boxPackStartDefaults kleineWelleVBox2 kleineWelleHBox12
  boxPackStartDefaults kleineWelleHBox12 kleineWelleHourEntryLable
  boxPackStartDefaults kleineWelleHBox12 kleineWelleMinuteEntryLable
  boxPackStartDefaults kleineWelleHBox12 kleineWelleSecondEntryLable
  boxPackStartDefaults kleineWelleVBox2 kleineWelleHBox13
  boxPackStartDefaults kleineWelleHBox13 kleineWelleHourEntry
  boxPackStartDefaults kleineWelleHBox13 kleineWelleMinuteEntry
  boxPackStartDefaults kleineWelleHBox13 kleineWelleSecondEntry

  boxPackStartDefaults kleineWelleVBox2 kleineWelleHBox2
  boxPackStartDefaults kleineWelleHBox2 kleineWelleObsEntryLable
  boxPackStartDefaults kleineWelleHBox2 kleineWelleObsEntry
  boxPackStartDefaults kleineWelleVBox2 kleineWelleHBox3
  boxPackStartDefaults kleineWelleHBox3 kleineWelleStrideEntryLable
  boxPackStartDefaults kleineWelleHBox3 kleineWelleStrideEntry
  boxPackStartDefaults kleineWelleVBox2 kleineWelleHBox4
  boxPackStartDefaults kleineWelleHBox4 kleineWelleSignificanceEntryLable
  boxPackStartDefaults kleineWelleHBox4 kleineWelleSignificanceEntry
  boxPackStartDefaults kleineWelleVBox2 kleineWelleHBox5
  boxPackStartDefaults kleineWelleHBox5 kleineWelleThresholdEntryLable
  boxPackStartDefaults kleineWelleHBox5 kleineWelleThresholdEntry
  boxPackStartDefaults kleineWelleVBox2 kleineWelleHBox6
  boxPackStartDefaults kleineWelleHBox6 kleineWelleLowCutOffEntryLable
  boxPackStartDefaults kleineWelleHBox6 kleineWelleLowCutOffEntry
  boxPackStartDefaults kleineWelleVBox2 kleineWelleHBox7
  boxPackStartDefaults kleineWelleHBox7 kleineWelleHighCutOffEntryLable
  boxPackStartDefaults kleineWelleHBox7 kleineWelleHighCutOffEntry

  boxPackStartDefaults kleineWelleVBox2 kleineWelleHBox0
  boxPackStartDefaults kleineWelleHBox0 kleineWelleClose
  boxPackStartDefaults kleineWelleHBox0 kleineWelleExecute


  {--  Execute --}
  onClicked kleineWelleExecute $ do
    putStrLn "Execute"
    let kwActiveLabels = HGGS.getActiveLabels kwChannelCButtons

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

    -- s_temp <- entryGetText kleineWelleGpsEntry
    -- let kwGpsTime = read s_temp :: Int
    -- putStrLn ("   GPS Time: " ++ (show kwGpsTime) )
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
    let kwCasheFile = "../sample-data/gwffiles_sorted.lst"
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


