{-******************************************
  *     File Name: GUI_GlitchKleineWelle.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/06/18 02:44:29
  *******************************************-}

module HasKAL.GUI_Utils.GUI_GlitchKleineWelle(
   hasKalGuiKleineWelle
) where

import Graphics.UI.Gtk
import qualified Control.Monad as CM -- forM
import qualified Data.Maybe as DM -- fromJust
import qualified System.IO.Unsafe as SIOU -- unsafePerformIO

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
      kwTransientDuration = 4.0
      kwDecimateFactor = -1
      kwUnowen_2 = 2
      kwOptFilePref = "optKW_"
      kwListFile = "tmpCachedFrameFile.lst"

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

  kleineWelleCacheOpener <- HGGS.fileOpenButtonNewWithLabelDefault "Cache file" $ HGGS.haskalOpt ++ "/cachefiles/gwffiles_sorted.lst"
  kleineWelleYearCombo <- HGGS.comboBoxNewLabelAppendTexts "Year" (map show [2010..2020]) 4
  kleineWelleMonthCombo <- HGGS.comboBoxNewLabelAppendTexts "Month" (map show [1..12]) 2
  kleineWelleDayCombo <- HGGS.comboBoxNewLabelAppendTexts "Day" (map show [1..31]) 16
  kleineWelleHourCombo <- HGGS.comboBoxNewLabelAppendTexts "Hour" (map show [0..23]) 16
  kleineWelleMinuteCombo <- HGGS.comboBoxNewLabelAppendTexts "Minute" (map show [0..59]) 15
  kleineWelleSecondCombo <- HGGS.comboBoxNewLabelAppendTexts "Second (JST)" (map show [0..59]) 12
  kleineWelleObsEntry <- HGGS.entryNewWithLabelDefault "OBS Time [s]" "128"
  kleineWelleStrideEntry <- HGGS.entryNewWithLabelDefault "Stride" "16"
  kleineWelleSignificanceEntry <- HGGS.entryNewWithLabelDefault "Significance" "2.0"
  kleineWelleThresholdEntry <- HGGS.entryNewWithLabelDefault "Threshold" "3.0"
  kleineWelleLowCutOffEntry <- HGGS.entryNewWithLabelDefault "LowCutOff [Hz]" "10"
  kleineWelleHighCutOffEntry <- HGGS.entryNewWithLabelDefault "HighCutOff [Hz]" "1000"

  kleineWelleClose <- buttonNewWithLabel "Close"
  kleineWelleExecute <- buttonNewWithLabel "Execute"

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
  HGGS.boxPackStartDefaultsPair kleineWelleHBoxCache kleineWelleCacheOpener
  boxPackStartDefaults kleineWelleVBox2 kleineWelleHBoxDateL
  mapM (boxPackStartDefaults kleineWelleHBoxDateL) $ map fst [kleineWelleYearCombo, kleineWelleMonthCombo, kleineWelleDayCombo]
  boxPackStartDefaults kleineWelleVBox2 kleineWelleHBoxDateE
  mapM (boxPackStartDefaults kleineWelleHBoxDateE) $ map snd [kleineWelleYearCombo, kleineWelleMonthCombo, kleineWelleDayCombo]
  boxPackStartDefaults kleineWelleVBox2 kleineWelleHBoxTimeL
  mapM (boxPackStartDefaults kleineWelleHBoxTimeL) $ map fst [kleineWelleHourCombo, kleineWelleMinuteCombo, kleineWelleSecondCombo]
  boxPackStartDefaults kleineWelleVBox2 kleineWelleHBoxTimeE
  mapM (boxPackStartDefaults kleineWelleHBoxTimeE) $ map snd [kleineWelleHourCombo, kleineWelleMinuteCombo, kleineWelleSecondCombo]
  boxPackStartDefaults kleineWelleVBox2 kleineWelleHBoxObs
  HGGS.boxPackStartDefaultsPair kleineWelleHBoxObs kleineWelleObsEntry
  boxPackStartDefaults kleineWelleVBox2 kleineWelleHBoxStride
  HGGS.boxPackStartDefaultsPair kleineWelleHBoxStride kleineWelleStrideEntry
  boxPackStartDefaults kleineWelleVBox2 kleineWelleHBoxSignificance
  HGGS.boxPackStartDefaultsPair kleineWelleHBoxSignificance kleineWelleSignificanceEntry
  boxPackStartDefaults kleineWelleVBox2 kleineWelleHBoxThreshold
  HGGS.boxPackStartDefaultsPair kleineWelleHBoxThreshold kleineWelleThresholdEntry
  boxPackStartDefaults kleineWelleVBox2 kleineWelleHBoxLowCutOff
  HGGS.boxPackStartDefaultsPair kleineWelleHBoxLowCutOff kleineWelleLowCutOffEntry
  boxPackStartDefaults kleineWelleVBox2 kleineWelleHBoxHighCutOff
  HGGS.boxPackStartDefaultsPair kleineWelleHBoxHighCutOff kleineWelleHighCutOffEntry

  boxPackStartDefaults kleineWelleVBox2 kleineWelleHBoxExecute
  mapM (boxPackStartDefaults kleineWelleHBoxExecute) [kleineWelleClose, kleineWelleExecute]

  {--  Execute --}
  onClicked kleineWelleExecute $ do
    putStrLn "Execute"
    let kwActiveLabels = HGGS.getActiveLabels kwChannelCButtons
        lwtColmunNum = length kwActiveLabels
        kwCasheFile = DM.fromJust $ SIOU.unsafePerformIO $ fileChooserGetFilename $ snd kleineWelleCacheOpener
        kwYear = read $ DM.fromJust $ SIOU.unsafePerformIO $ comboBoxGetActiveText $ snd kleineWelleYearCombo :: Int
        kwMonth = read $ DM.fromJust $ SIOU.unsafePerformIO $ comboBoxGetActiveText $ snd kleineWelleMonthCombo :: Int
        kwDay = read $ DM.fromJust $ SIOU.unsafePerformIO $ comboBoxGetActiveText $ snd kleineWelleDayCombo :: Int
        kwHour = read $ DM.fromJust $ SIOU.unsafePerformIO $ comboBoxGetActiveText $ snd kleineWelleHourCombo :: Int
        kwMinute = read $ DM.fromJust $ SIOU.unsafePerformIO $ comboBoxGetActiveText $ snd kleineWelleMinuteCombo :: Int
        kwSecond = read $ DM.fromJust $ SIOU.unsafePerformIO $ comboBoxGetActiveText $ snd kleineWelleSecondCombo :: Int
        kleineWelleDateStr = HGGS.iDate2sDate kwYear kwMonth kwDay kwHour kwMinute kwSecond
        kwGpsTime = read $ HTG.time2gps kleineWelleDateStr :: Int
        kwObsTime = read $ SIOU.unsafePerformIO $ entryGetText $ snd kleineWelleObsEntry :: Integer
        kwStride = read $ SIOU.unsafePerformIO $ entryGetText $ snd kleineWelleStrideEntry :: Int
        kwSignificance = read $ SIOU.unsafePerformIO $ entryGetText $ snd kleineWelleSignificanceEntry :: Double
        kwThreshold = read $ SIOU.unsafePerformIO $ entryGetText $ snd kleineWelleThresholdEntry :: Double
        kwLowCutOff = read $ SIOU.unsafePerformIO $ entryGetText $ snd kleineWelleLowCutOffEntry :: Int
        kwHighCutOff = read $ SIOU.unsafePerformIO $ entryGetText $ snd kleineWelleHighCutOffEntry :: Int
    putStrLn ("   JST Time: " ++ kleineWelleDateStr)
    putStrLn ("   GPS Time: " ++ (show kwGpsTime))
    putStrLn ("   Obs Time: " ++ (show kwObsTime) )
    putStrLn ("   Stride: " ++ (show kwStride) )
    putStrLn ("   Significance: " ++ (show kwSignificance) )
    putStrLn ("   Threshold: " ++ (show kwThreshold) )
    putStrLn ("   LowCutOff: " ++ (show kwLowCutOff) )
    putStrLn ("   HighCutOff: " ++ (show kwHighCutOff) )
    putStrLn "   Channels: "
    mapM_ putStrLn kleineWelleActiveLabels
    putStrLn "   Column: "
    mapM_ putStrLn kwActiveLabels
    putStrLn $ (show (fromIntegral kwGpsTime)) ++ " ~ " ++ (show (fromIntegral kwGpsTime + kwObsTime))
{--}
    HFP.pickUpFileNameinoutFile (fromIntegral kwGpsTime) (fromIntegral kwGpsTime + kwObsTime) kwCasheFile
    putStrLn "Generate optM file for KleineWelle"
    lwtOutput <- HMKKW.execKleineWelle kwStride kwBasename kwTransientDuration kwSignificance kwThreshold kwDecimateFactor kleineWelleActiveLabels kwLowCutOff kwHighCutOff kwUnowen_2 kwOptFilePref kwListFile kwGpsTime kwActiveLabels
{----}
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


