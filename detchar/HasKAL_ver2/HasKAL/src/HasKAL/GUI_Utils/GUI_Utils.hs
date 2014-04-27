{-******************************************************************
  *     File Name: GUI_Utils.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/04/18 16:20:58
  ******************************************************************-}

module HasKAL.GUI_Utils.GUI_Utils
  (hasKalGuiTop
  ) where

import Graphics.UI.Gtk
import qualified System.IO as SIO -- openFile
import qualified Control.Monad as CM -- forM
import qualified Text.Regex as TR -- splitRegex, mkRegex
import qualified Text.Printf as TP -- printf
import qualified System.IO.Unsafe as SIOU -- unsafePerformIO

import qualified HasKAL.FrameUtils.FrameUtils as HFF
import qualified HasKAL.MonitorUtils.EXTKleineWelle as HMK
import qualified HasKAL.MonitorUtils.RayleighMon as HMR
import qualified HasKAL.TimeUtils.GPSfunction as HTG
import qualified HasKAL.PlotUtils.PlotUtils as HPP
import qualified HasKAL.PlotUtils.PlotUtilsHROOT as HPPR

hasKalGuiTop :: IO ()
hasKalGuiTop = do
  initGUI

  {--  information  --}
  let topSubSystemLabels = ["Test", "TUN", "FCL", "VAC", "CRY", "VIS", "MIR", "LAS", "MIF", "IOO", "AOS", "AEL", "DGS", "DAS", "GIF", "DC"] -- sub system names (ハードコーディングで良いか？
  let topNumOfSubSystems = length topSubSystemLabels -- number of sub systems
  let topMonitorLabels = ["Glitch", "Line", "Gaussianity"] -- monitor names (ハードコーディングで良いか？

  {--  Create new object --}
  topWindow <- windowNew -- main window
  topSubSystemVbox <- vBoxNew True 10 -- vbox for inserting sub system scroll window
  topMonitorVbox <- vBoxNew True 10 -- vbox for inserting monitor buttons
  topExitVbox <- vBoxNew True 10 -- vbox for inserting exit button
  topSubSystemScroll <- scrolledWindowNew Nothing Nothing -- scroll window for inserting sub system button box
  topSubSystemButtonBox <- vButtonBoxNew -- button box for gathering sub system buttons as one object
  topSubSystemCheckButtons <- mapM checkButtonNewWithLabel topSubSystemLabels -- check buttons of sub system
  topMonitorButtons <- mapM buttonNewWithLabel topMonitorLabels -- buttons of monitors
  topExitButton <- buttonNewWithLabel "Exit" -- button of exit

  {--  Set Parameters of the objects  --}
  set topWindow [ windowTitle := "HasKAL_GUI",
                      windowDefaultWidth := 200,
                      windowDefaultHeight := 450,
                      containerChild := topSubSystemVbox,
                      containerBorderWidth := 20 ]
  scrolledWindowSetPolicy topSubSystemScroll PolicyAutomatic PolicyAutomatic

  {--  Arrange object in window  --}
  mapM (boxPackStartDefaults topSubSystemButtonBox) topSubSystemCheckButtons -- insert sub system buttons in button box
  scrolledWindowAddWithViewport topSubSystemScroll topSubSystemButtonBox -- insert button box in scroll window
  boxPackStartDefaults topSubSystemVbox topSubSystemScroll -- insert vbox in scroll window
  boxPackStartDefaults topSubSystemVbox topMonitorVbox -- create small vbox
  mapM (boxPackStartDefaults topMonitorVbox) topMonitorButtons -- insert monitor buttons in vbox
  boxPackStartDefaults topMonitorVbox topExitVbox -- create small vbox
  boxPackStartDefaults topExitVbox topExitButton -- insert exit button in vbox

   {--  Select Glitch Monitor --}
  onClicked (topMonitorButtons !! 0) $ do
    putStrLn =<< fmap (++ " Monitor: New window open.") (buttonGetLabel (topMonitorButtons !! 0))
    let topActiveLabels = getActiveLabels topSubSystemCheckButtons
    hasKalGuiGlitch topActiveLabels
  {--  Select Line Monitor --}
  onClicked (topMonitorButtons !! 1) $ do
    putStrLn =<< fmap (++ " Monitor: Not implemented yet.") (buttonGetLabel (topMonitorButtons !! 1))
  {--  Select Gaussianity Monitor --}
  onClicked (topMonitorButtons !! 2) $ do
    putStrLn =<< fmap (++ " Monitor: New window open.") (buttonGetLabel (topMonitorButtons !! 2))
    let topActiveLabels = getActiveLabels topSubSystemCheckButtons
    hasKalGuiGaussianity topActiveLabels
  {--  Select Exit  --}
  onClicked topExitButton $ do
    putStrLn "Exit"
    widgetDestroy topWindow

  {--  Exit Process  --}
  onDestroy topWindow mainQuit
  widgetShowAll topWindow
  mainGUI



{- Glitch Monitors -}
hasKalGuiGlitch :: [String] -> IO ()
hasKalGuiGlitch activeSubSystemlabels = do
  initGUI

  {--  Create new object --}
  glitchWindow <- windowNew
  glitchVBox <- vBoxNew True 5
  glitchVBox2 <- vBoxNew True 5

  {--  Read file of channel list  --}
  glitchChannels <- CM.forM activeSubSystemlabels $ \lambda -> SIO.hGetContents =<< SIO.openFile ("../ChList/channelList" ++ lambda ++ ".txt") SIO.ReadMode --glitchIFile

  {--  Information  --}
  let glitchMonitorLabels = ["KleineWelle"]
  let glitchChannelLabels = lines (concat glitchChannels)
  let glitchNumOfChannel = length glitchChannelLabels

  glitchChannelScroll <- scrolledWindowNew Nothing Nothing
  glitchChannelBBox <- vButtonBoxNew
  glitchChannelCButtons <- mapM checkButtonNewWithLabel glitchChannelLabels
  glitchMonitorButtons <- mapM buttonNewWithLabel glitchMonitorLabels
  glitchCloseButton <- buttonNewWithLabel "Close"

  {--  Set Parameters of the objects  --}
  set glitchWindow [ windowTitle := "Glitch Monitor",
                      windowDefaultWidth := 200,
                      windowDefaultHeight := 450,
                      containerChild := glitchVBox,
                      containerBorderWidth := 20 ]
  scrolledWindowSetPolicy glitchChannelScroll PolicyAutomatic PolicyAutomatic

  {--  Arrange object in window  --}
  mapM (boxPackStartDefaults glitchChannelBBox) glitchChannelCButtons
  scrolledWindowAddWithViewport glitchChannelScroll glitchChannelBBox
  boxPackStartDefaults glitchVBox glitchChannelScroll
  boxPackStartDefaults glitchVBox glitchVBox2
  mapM (boxPackStartDefaults glitchVBox2) glitchMonitorButtons
  boxPackStartDefaults glitchVBox2 glitchCloseButton

   {--  Select Glitch Monitor --}
  onClicked (glitchMonitorButtons !! 0) $ do
    putStrLn =<< fmap (++ ": New window open.") (buttonGetLabel (glitchMonitorButtons !! 0))
    let glitchActiveLabels = getActiveLabels glitchChannelCButtons
    hasKalGuiKleineWelle glitchActiveLabels
  {--  Select Closed  --}
  onClicked glitchCloseButton $ do
    putStrLn "Closed Glitch window"
    widgetDestroy glitchWindow

  {--  Exit Process  --}
  onDestroy glitchWindow mainQuit
  widgetShowAll glitchWindow
  mainGUI


hasKalGuiKleineWelle :: [String] -> IO ()
hasKalGuiKleineWelle kleineWelleActiveLabels = do
  initGUI

  {--  Fixed value for KleineWelle  --}
  let kwBasename = "KW_"
  let kwTransientDuration = 4.0
  let kwDecimateFactor = -1
  let kwUnowen_2 = 2
  let kwOptFilePref = "optKW_"
  let kwListFile = "gwffilelist.txt"

  {--  Read file of channel list  --}
  --kleineWelleChannels <- forM activeSubSystemlabels $ \lambda -> hGetContents =<< openFile ("../ChList/channelList" ++ lambda ++ ".txt") ReadMode --kleineWelleIFile

  {--  for lwtprint  --}
  let kwChannelLabels = [ "ifo", "peak_time", "peak_time_ns", "start_time", "start_time_ns", "duration", "search", "central_freq", "channel", "amplitude", "snr", "confidence", "chisq", "chisq_dof", "bandwidth", "event_id", "process_id", "table" ]
  kwChannelScroll <- scrolledWindowNew Nothing Nothing
  kwChannelBBox <- vButtonBoxNew
  kwChannelCButtons <- mapM checkButtonNewWithLabel kwChannelLabels


  {--  Information  --}
  --let kleineWelleChannelLabels = lines (concat kleineWelleChannels)
  --let kleineWelleNumOfChannel = length kleineWelleChannelLabels
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
  kleineWelleGpsEntryLable <- labelNewWithMnemonic "GPS Time [s]"
{--}
  kleineWelleYearEntryLable <- labelNewWithMnemonic "年"
  kleineWelleMonthEntryLable <- labelNewWithMnemonic "月"
  kleineWelleDayEntryLable <- labelNewWithMnemonic "日"
  kleineWelleHourEntryLable <- labelNewWithMnemonic "時"
  kleineWelleMinuteEntryLable <- labelNewWithMnemonic "分"
  kleineWelleSecondEntryLable <- labelNewWithMnemonic "秒 (JST)"
{----}
  kleineWelleObsEntryLable <- labelNewWithMnemonic "OBS Time [s]"
  kleineWelleStrideEntryLable <- labelNewWithMnemonic "Stride"
  kleineWelleSignificanceEntryLable <- labelNewWithMnemonic "Significance"
  kleineWelleThresholdEntryLable <- labelNewWithMnemonic "Threshold"
  kleineWelleLowCutOffEntryLable <- labelNewWithMnemonic "LowCutOff [Hz]"
  kleineWelleHighCutOffEntryLable <- labelNewWithMnemonic "HighCutOff [Hz]"
  kleineWelleGpsEntry <- entryNew
{--}
  kleineWelleYearEntry <- entryNew
  kleineWelleMonthEntry <- entryNew
  kleineWelleDayEntry <- entryNew
  kleineWelleHourEntry <- entryNew
  kleineWelleMinuteEntry <- entryNew
  kleineWelleSecondEntry <- entryNew
{----}
  kleineWelleObsEntry <- entryNew
  kleineWelleStrideEntry <- entryNew
  kleineWelleSignificanceEntry <- entryNew
  kleineWelleThresholdEntry <- entryNew
  kleineWelleLowCutOffEntry <- entryNew
  kleineWelleHighCutOffEntry <- entryNew
  kleineWelleClose <- buttonNewWithLabel "Close"
  kleineWelleExecute <- buttonNewWithLabel "Execute"
  entrySetText kleineWelleGpsEntry "1066392016"
{--}
  entrySetText kleineWelleYearEntry "2013"
  entrySetText kleineWelleMonthEntry "10"
  entrySetText kleineWelleDayEntry "21"
  entrySetText kleineWelleHourEntry "21"
  entrySetText kleineWelleMinuteEntry "0"
  entrySetText kleineWelleSecondEntry "0"
{----}
  entrySetText kleineWelleObsEntry "300"
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
  --mapM (boxPackStartDefaults kleineWelleChannelBBox) kleineWelleChannelCButtons
  --scrolledWindowAddWithViewport kleineWelleChannelScroll kleineWelleChannelBBox
  --boxPackStartDefaults kleineWelleVBox kleineWelleHBox00
  --boxPackStartDefaults kleineWelleHBox00 kleineWelleChannelScroll

  mapM (boxPackStartDefaults kwChannelBBox) kwChannelCButtons
  scrolledWindowAddWithViewport kwChannelScroll kwChannelBBox
  boxPackStartDefaults kleineWelleHBox00 kwChannelScroll

  boxPackStartDefaults kleineWelleHBox00 kleineWelleVBox2
  boxPackStartDefaults kleineWelleVBox2 kleineWelleHBox

  -- boxPackStartDefaults kleineWelleHBox kleineWelleGpsEntryLable
  -- boxPackStartDefaults kleineWelleHBox kleineWelleGpsEntry
{--}
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
{----}
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
  onClicked kleineWelleClose $ do
    putStrLn "Close KleineWelle Monitor\n"
    widgetDestroy kleineWelleWindow
  onClicked kleineWelleExecute $ do
    putStrLn "Execute"
    --let kleineWelleActiveLabels = getActiveLabels kleineWelleChannelCButtons
    let kwActiveLabels = getActiveLabels kwChannelCButtons
{--}
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
    let kleineWelleDateStr = iDate2sDate kwYear kwMonth kwDay kwHour kwMinute kwSecond
    putStrLn ("   JST Time: " ++ kleineWelleDateStr)
    let hogeGPS = HTG.time2gps kleineWelleDateStr
    putStrLn ("   GPS Time: " ++ hogeGPS)
{----}
    s_temp <- entryGetText kleineWelleGpsEntry
    let kwGpsTime = read s_temp :: Int
    putStrLn ("   GPS Time: " ++ (show kwGpsTime) )
    s_temp <- entryGetText kleineWelleObsEntry
    let kwObsTime = read s_temp :: Int
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
    putStrLn "Generate optM file for KleineWelle"
    lwtOutput <- HMK.execKleineWelle kwStride kwBasename kwTransientDuration kwSignificance kwThreshold kwDecimateFactor kleineWelleActiveLabels kwLowCutOff kwHighCutOff kwUnowen_2 kwOptFilePref kwListFile kwGpsTime kwActiveLabels
    let lwtColmunNum = length kwActiveLabels
    putStrLn "Run Plot tool"
    if lwtColmunNum == 2
      then CM.forM lwtOutput $ \lambda -> HPP.scatter_plot_2d "TITLE 2" "HOGEHOGE" 10.0 (640,480) (convert_StoDT2L lambda)
      else if lwtColmunNum == 3
        then CM.forM lwtOutput $ \lambda -> HPP.scatter_plot_3d "TITLE 3" "HOGEHOGE" 10.0 (640,480) (convert_StoDT3L lambda)
        else mapM putStrLn ["Required 2 or 3 columns\n"]
    putStrLn "Close KleineWelle\n"
    widgetDestroy kleineWelleWindow
  {--  Exit Process  --}
  onDestroy kleineWelleWindow mainQuit
  widgetShowAll kleineWelleWindow
  mainGUI



{-- Gaussianity Monitors --}
hasKalGuiGaussianity :: [String] -> IO ()
hasKalGuiGaussianity activeSubSystemlabels = do
  initGUI

  {--  Create new object --}
  gaussianityWindow <- windowNew
  gaussianityVBox <- vBoxNew True 5
  gaussianityVBox2 <- vBoxNew True 5

  {--  Read file of channel list  --}
  gaussianityChannels <- CM.forM activeSubSystemlabels $ \lambda -> SIO.hGetContents =<< SIO.openFile ("../ChList/channelList" ++ lambda ++ ".txt") SIO.ReadMode --gaussianityIFile

  {--  Information  --}
  let gaussianityMonitorLabels = ["RayleighMon"]
  let gaussianityChannelLabels = lines (concat gaussianityChannels)
  let gaussianityNumOfChannel = length gaussianityChannelLabels

  gaussianityChannelScroll <- scrolledWindowNew Nothing Nothing
  gaussianityChannelBBox <- vButtonBoxNew
  gaussianityChannelCButtons <- mapM checkButtonNewWithLabel gaussianityChannelLabels
  gaussianityMonitorButtons <- mapM buttonNewWithLabel gaussianityMonitorLabels
  gaussianityCloseButton <- buttonNewWithLabel "Close"

  {--  Set Parameters of the objects  --}
  set gaussianityWindow [ windowTitle := "Gaussianity Monitor",
                      windowDefaultWidth := 200,
                      windowDefaultHeight := 450,
                      containerChild := gaussianityVBox,
                      containerBorderWidth := 20 ]
  scrolledWindowSetPolicy gaussianityChannelScroll PolicyAutomatic PolicyAutomatic

  {--  Arrange object in window  --}
  mapM (boxPackStartDefaults gaussianityChannelBBox) gaussianityChannelCButtons
  scrolledWindowAddWithViewport gaussianityChannelScroll gaussianityChannelBBox
  boxPackStartDefaults gaussianityVBox gaussianityChannelScroll
  boxPackStartDefaults gaussianityVBox gaussianityVBox2
  mapM (boxPackStartDefaults gaussianityVBox2) gaussianityMonitorButtons
  boxPackStartDefaults gaussianityVBox2 gaussianityCloseButton

   {--  Select RayleighMon  --}
  onClicked (gaussianityMonitorButtons !! 0) $ do
    putStrLn =<< fmap (++ ": New window open.") (buttonGetLabel (gaussianityMonitorButtons !! 0))
    let gaussianityActiveLabels = getActiveLabels gaussianityChannelCButtons
    hasKalGuiRayleighMon gaussianityActiveLabels
  {--  Select Closed  --}
  onClicked gaussianityCloseButton $ do
    putStrLn "Closed Gaussianity window"
    widgetDestroy gaussianityWindow

  {--  Exit Process  --}
  onDestroy gaussianityWindow mainQuit
  widgetShowAll gaussianityWindow
  mainGUI


hasKalGuiRayleighMon :: [String] -> IO ()
hasKalGuiRayleighMon activeChannelLabels = do
  initGUI  

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


  rayleighMonYearEntryLable <- labelNewWithMnemonic "年"
  rayleighMonMonthEntryLable <- labelNewWithMnemonic "月"
  rayleighMonDayEntryLable <- labelNewWithMnemonic "日"
  rayleighMonHourEntryLable <- labelNewWithMnemonic "時"
  rayleighMonMinuteEntryLable <- labelNewWithMnemonic "分"
  rayleighMonSecondEntryLable <- labelNewWithMnemonic "秒 (JST)"
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
    putStrLn "Close RayleighMon\n"
    widgetDestroy rayleighMonWindow
  onClicked rayleighMonExecute $ do
    putStrLn "Execute"

    s_temp <- entryGetText rayleighMonYearEntry
    let rmYear = read s_temp :: Int
    s_temp <- entryGetText rayleighMonMonthEntry
    let rmMonth = read s_temp :: Int
    s_temp <- entryGetText rayleighMonDayEntry
    let rmDay = read s_temp :: Int
    s_temp <- entryGetText rayleighMonHourEntry
    let rmHour = read s_temp :: Int
    s_temp <- entryGetText rayleighMonMinuteEntry
    let rmMinute = read s_temp :: Int
    s_temp <- entryGetText rayleighMonSecondEntry
    let rmSecond = read s_temp :: Int

    let rayleighMonDateStr = iDate2sDate rmYear rmMonth rmDay rmHour rmMinute rmSecond
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
    HPPR.hroot_core (map fromIntegral [0,1..(rmStride `div` 2 + 1)]) ( (transposed $ HMR.rayleighMon rmStride rmStride rmSampling (map realToFrac (HFF.eval frData))) !! 0) "frequency [Hz]" "noise level [/rHz]" HPPR.LogXY HPPR.Line
    -- 横軸の値を直す(1秒スペクトルなので今は正しい)

    {-- 暫定的なファイル出力 --}
    oFile <- SIO.openFile "./GUI-RayleighMon_Result.txt" SIO.WriteMode
    SIO.hPutStrLn oFile (convert_DLL2S $ HMR.rayleighMon rmStride rmStride rmSampling (map realToFrac (HFF.eval frData)) )
    SIO.hClose oFile
    {--  ここまで、ファイル出力  --}



    widgetDestroy rayleighMonWindow


  {--  Exit Process  --}
  onDestroy rayleighMonWindow mainQuit
  widgetShowAll rayleighMonWindow
  mainGUI






{-- Supplementary Functions --}
getActiveLabels :: [CheckButton] -> [String]
getActiveLabels [] = []
getActiveLabels (x:xs) = 
  case SIOU.unsafePerformIO (toggleButtonGetActive x) of 
    True -> (SIOU.unsafePerformIO (buttonGetLabel x)):(getActiveLabels xs)
    False -> getActiveLabels xs


convert_StoD :: String -> Double
convert_StoD = read

convert_LtoT2 :: [Double] -> (Double, Double)
convert_LtoT2 [x, y] = (x, y)

convert_LtoT3 :: [Double] -> (Double, Double, Double)
convert_LtoT3 [x, y, z] = (x, y, z)

convert_StoDT2L :: String -> [(Double, Double)]
convert_StoDT2L stringData = map convert_LtoT2 (map (map convert_StoD) (map (TR.splitRegex (TR.mkRegex ",")) (lines stringData) ) )

convert_StoDT3L :: String -> [(Double, Double, Double)]
convert_StoDT3L stringData = map convert_LtoT3 (map (map convert_StoD) (map (TR.splitRegex (TR.mkRegex ",")) (lines stringData) ) )

convert_DLL2S :: [[Double]] -> String
convert_DLL2S xss = unlines [unwords (map show xs) | xs <- xss]

iDate2sDate :: Int -> Int -> Int -> Int -> Int -> Int -> String
iDate2sDate intYear intMonth intDay intHour intMinute intSecond =
              (TP.printf "%04d" intYear :: String) ++ "-" ++ (TP.printf "%02d" intMonth :: String) ++ "-" ++ (TP.printf "%02d" intDay :: String) ++ " " ++ (TP.printf "%02d" intHour :: String) ++ ":" ++ (TP.printf "%02d" intMinute :: String) ++ ":" ++ (TP.printf "%02d" intSecond :: String) ++ " JST"

-- transposed 2D list
transposed :: [[Double]] -> [[Double]]
transposed xxs = [ concat $ map ((drop (m-1)).(take m)) xxs | m <- [1..(length (head xxs))] ]
-- [ [h_1(f=0), h_1(f=1), ..], [h_2(f=0), h_2(f=1), ..], ..] -> [ [h_1(f=0), h_2(f=0), ..], [h_1(f=1), h_2(f=1), ..], ..]
-- same as in RayleighMon

