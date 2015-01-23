{- |
Module      : HasKAL.GUI_Utils.GUI_GlitchKleineWelle
Description : This is documentation tests.
Copyright   : (c) WhoAmI, 2014
License     : ???
Maintainer  : hoge@hoge.com
Stability   : test
Portability : POSIX

GUI of KleineWelle

-}

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
  kleineWelleVBox2 <- vBoxNew True 5
  kleineWelleHBoxCache <- hBoxNew True 5
  kleineWelleHBoxScroll <- hBoxNew True 5
  kleineWelleHBoxDate <- mapM (hBoxNew True) $ take 7 [5..]
  kleineWelleHBoxObs <- hBoxNew True 5
  kleineWelleHBoxStride <- hBoxNew True 5
  kleineWelleHBoxSignificance <- hBoxNew True 5
  kleineWelleHBoxThreshold <- hBoxNew True 5
  kleineWelleHBoxLowCutOff <- hBoxNew True 5
  kleineWelleHBoxHighCutOff <- hBoxNew True 5
  kleineWelleHBoxExecute <- hBoxNew True 5
  kleineWelleChannelScroll <- scrolledWindowNew Nothing Nothing
  kleineWelleChannelBBox <- vButtonBoxNew

  kleineWelleCacheOpener <- HGGS.fileOpenButtonNewWithLabelDefault "Cache file" $ HGGS.haskalOpt ++ "/cachefiles/cachefile_20140702.lst"
  kleineWelleDateCombo <- HGGS.dateComboNew (2014, 5, 17, 16, 15, 12, "JST")
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
  mapM (\x -> boxPackStart kwChannelBBox x PackGrow 0) kwChannelCButtons
  scrolledWindowAddWithViewport kwChannelScroll kwChannelBBox
  boxPackStart kleineWelleHBoxScroll kwChannelScroll PackGrow 0
  boxPackStart kleineWelleHBoxScroll kleineWelleVBox2 PackGrow 0

  boxPackStart kleineWelleVBox2 kleineWelleHBoxCache PackGrow 0
  HGGS.boxPackStartDefaultsPair kleineWelleHBoxCache kleineWelleCacheOpener
  mapM (\x -> boxPackStart kleineWelleVBox2 x PackGrow 0) kleineWelleHBoxDate
  CM.zipWithM HGGS.boxPackStartDefaultsPair kleineWelleHBoxDate $ kleineWelleDateCombo

  boxPackStart kleineWelleVBox2 kleineWelleHBoxObs PackGrow 0
  HGGS.boxPackStartDefaultsPair kleineWelleHBoxObs kleineWelleObsEntry
  boxPackStart kleineWelleVBox2 kleineWelleHBoxStride PackGrow 0
  HGGS.boxPackStartDefaultsPair kleineWelleHBoxStride kleineWelleStrideEntry
  boxPackStart kleineWelleVBox2 kleineWelleHBoxSignificance PackGrow 0
  HGGS.boxPackStartDefaultsPair kleineWelleHBoxSignificance kleineWelleSignificanceEntry
  boxPackStart kleineWelleVBox2 kleineWelleHBoxThreshold PackGrow 0
  HGGS.boxPackStartDefaultsPair kleineWelleHBoxThreshold kleineWelleThresholdEntry
  boxPackStart kleineWelleVBox2 kleineWelleHBoxLowCutOff PackGrow 0
  HGGS.boxPackStartDefaultsPair kleineWelleHBoxLowCutOff kleineWelleLowCutOffEntry
  boxPackStart kleineWelleVBox2 kleineWelleHBoxHighCutOff PackGrow 0
  HGGS.boxPackStartDefaultsPair kleineWelleHBoxHighCutOff kleineWelleHighCutOffEntry
  boxPackStart kleineWelleVBox2 kleineWelleHBoxExecute PackGrow 0
  mapM (\x -> boxPackStart kleineWelleHBoxExecute x PackGrow 0) [kleineWelleClose, kleineWelleExecute]

  {--  Execute --}
  onClicked kleineWelleExecute $ do
    putStrLn "Execute"
    let kwActiveLabels = HGGS.getActiveLabels kwChannelCButtons
        lwtColmunNum = length kwActiveLabels
        kwCasheFile = DM.fromJust $ SIOU.unsafePerformIO $ fileChooserGetFilename $ snd kleineWelleCacheOpener
    kwDate <- CM.liftM HGGS.dateStr2Tuple $ mapM HGGS.comboBoxGetActiveString kleineWelleDateCombo
    let kwGpsTime = read $ HTG.timetuple2gps kwDate :: Integer
        kwObsTime = read $ SIOU.unsafePerformIO $ entryGetText $ snd kleineWelleObsEntry :: Integer
        kwStride = read $ SIOU.unsafePerformIO $ entryGetText $ snd kleineWelleStrideEntry :: Int
        kwSignificance = read $ SIOU.unsafePerformIO $ entryGetText $ snd kleineWelleSignificanceEntry :: Double
        kwThreshold = read $ SIOU.unsafePerformIO $ entryGetText $ snd kleineWelleThresholdEntry :: Double
        kwLowCutOff = read $ SIOU.unsafePerformIO $ entryGetText $ snd kleineWelleLowCutOffEntry :: Int
        kwHighCutOff = read $ SIOU.unsafePerformIO $ entryGetText $ snd kleineWelleHighCutOffEntry :: Int
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
    putStrLn $ (show kwGpsTime) ++ " ~ " ++ (show $ kwGpsTime + kwObsTime)
{--}
    HFP.pickUpFileNameinoutFile kwGpsTime (kwGpsTime + kwObsTime) kwCasheFile
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


