{-******************************************************************
  *     File Name: GUI_Utils.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/05/13 17:13:05
  ******************************************************************-}

module HasKAL.GUI_Utils.GUI_Utils
  (hasKalGuiTop
  ) where

import Graphics.UI.Gtk
import qualified Control.Monad as CM -- forM
import qualified Numeric.LinearAlgebra as NLA -- data Vector, fromList, toList
import qualified System.IO as SIO -- openFile
import qualified System.IO.Unsafe as SIOU -- unsafePerformIO
import qualified Text.Regex as TR -- splitRegex, mkRegex
import qualified Text.Printf as TP -- printf

import qualified HasKAL.DetectorUtils.Detector as HDD
import qualified HasKAL.FrameUtils.FrameUtils as HFF
import qualified HasKAL.MonitorUtils.KleineWelle.EXTKleineWelle as HMKKW
import qualified HasKAL.MonitorUtils.RayleighMon.RayleighMon as HMRRM
import qualified HasKAL.MonitorUtils.RangeMon.InspiralRingdownDistance as HMRIRD
import qualified HasKAL.PlotUtils.PlotUtils as HPP
import qualified HasKAL.PlotUtils.PlotOption.PlotOptionHROOT as HPPOR
import qualified HasKAL.PlotUtils.PlotUtilsHROOT as HPPR
import qualified HasKAL.TimeUtils.GPSfunction as HTG

{-- GUI main window
-- test code
main :: IO ()
main = hasKalGuiTop
-- arguments: Nothing
-}
hasKalGuiTop :: IO ()
hasKalGuiTop = do
  initGUI
  putStrLn "Start HasKAL GUI"

  {--  information  --}
  let topSubSystemLabels = ["Test", "TUN", "FCL", "VAC", "CRY", "VIS", "MIR", "LAS", "MIF", "IOO", "AOS", "AEL", "DGS", "DAS", "GIF", "DC"] -- sub system names
  let topNumOfSubSystems = length topSubSystemLabels -- number of sub systems
  let topMonitorLabels = ["Glitch", "Line", "Gaussianity", "RangeMon"] -- monitor names

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
    let topActiveLabels = getActiveLabels topSubSystemCheckButtons
    case length topActiveLabels of 0 -> hasKalGuiMessage "Error" "Not selected subsystem."
                                   _ -> hasKalGuiGlitch topActiveLabels
    widgetDestroy topWindow
    hasKalGuiTop
  {--  Select Line Monitor --}
  onClicked (topMonitorButtons !! 1) $ do
    let topActiveLabels = getActiveLabels topSubSystemCheckButtons
    case length topActiveLabels of 0 -> hasKalGuiMessage "Error" "Not selected subsystem."
                                   _ -> hasKalGuiMessage "Error" "Not implemented yet."
    widgetDestroy topWindow
    hasKalGuiTop
  {--  Select Gaussianity Monitor --}
  onClicked (topMonitorButtons !! 2) $ do
    let topActiveLabels = getActiveLabels topSubSystemCheckButtons
    case length topActiveLabels of 0 -> hasKalGuiMessage "Error" "Not selected subsystem."
                                   _ -> hasKalGuiGaussianity topActiveLabels
    widgetDestroy topWindow
    hasKalGuiTop
  {--  Select Range Monitor --}
  onClicked (topMonitorButtons !! 3) $ do
    hasKalGuiRangeMon
  {--  Select Exit  --}
  onClicked topExitButton $ do
    putStrLn "Exit HasKAL GUI"
    widgetDestroy topWindow

  {--  Exit Process  --}
  onDestroy topWindow mainQuit
  widgetShowAll topWindow
  mainGUI



{- Glitch Monitors Window
-- test code
main :: IO ()
main = hasKalGuiGlitch ["Test"]
-- arguments: Subsystem_Name
-}
hasKalGuiGlitch :: [String] -> IO ()
hasKalGuiGlitch activeSubSystemlabels = do
  initGUI
  putStrLn "Open Glitch Monitors Window"

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
    let glitchActiveLabels = getActiveLabels glitchChannelCButtons
    case length glitchActiveLabels of 0 -> hasKalGuiMessage "Error" "Not selected channels"
                                      _ -> hasKalGuiKleineWelle glitchActiveLabels
    widgetDestroy glitchWindow
    hasKalGuiGlitch activeSubSystemlabels
  {--  Select Closed  --}
  onClicked glitchCloseButton $ do
    putStrLn "Closed Glitch Monitors Window"
    widgetDestroy glitchWindow

  {--  Exit Process  --}
  onDestroy glitchWindow mainQuit
  widgetShowAll glitchWindow
  mainGUI


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
  kleineWelleYearEntryLable <- labelNewWithMnemonic "Year"
  kleineWelleMonthEntryLable <- labelNewWithMnemonic "Month"
  kleineWelleDayEntryLable <- labelNewWithMnemonic "Day"
  kleineWelleHourEntryLable <- labelNewWithMnemonic "Hour"
  kleineWelleMinuteEntryLable <- labelNewWithMnemonic "Minute"
  kleineWelleSecondEntryLable <- labelNewWithMnemonic "Second (JST)"
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
  onClicked kleineWelleExecute $ do
    putStrLn "Execute"
    --let kleineWelleActiveLabels = getActiveLabels kleineWelleChannelCButtons
    let kwActiveLabels = getActiveLabels kwChannelCButtons
{--}
    s_temp <- entryGetText kleineWelleYearEntry
    -- if s_temp == ""
    -- then do widgetDestroy kleineWelleWindow
    --         hasKalGuiKleineWelle kleineWelleActiveLabels
    -- else do return ()
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
    lwtOutput <- HMKKW.execKleineWelle kwStride kwBasename kwTransientDuration kwSignificance kwThreshold kwDecimateFactor kleineWelleActiveLabels kwLowCutOff kwHighCutOff kwUnowen_2 kwOptFilePref kwListFile kwGpsTime kwActiveLabels
    let lwtColmunNum = length kwActiveLabels
    putStrLn "Run Plot tool"
    case lwtColmunNum of 2 -> CM.forM lwtOutput $ \lambda -> HPP.scatter_plot_2d "TITLE 2" "HOGEHOGE" 10.0 (640,480) (convert_StoDT2L lambda)
                         3 -> CM.forM lwtOutput $ \lambda -> HPP.scatter_plot_3d "TITLE 3" "HOGEHOGE" 10.0 (640,480) (convert_StoDT3L lambda)
                         _ -> mapM putStrLn ["Required 2 or 3 columns"]
    return ()
  onClicked kleineWelleClose $ do
    putStrLn "Closed KleineWelle Monitor"
    widgetDestroy kleineWelleWindow

  {--  Exit Process  --}
  onDestroy kleineWelleWindow mainQuit
  widgetShowAll kleineWelleWindow
  mainGUI



{-- Gaussianity Monitors Window
-- test code
main = IO ()
main = hasKalGuiGaussianity ["Test"]
-- arguments: subsystem_name
--}
hasKalGuiGaussianity :: [String] -> IO ()
hasKalGuiGaussianity activeSubSystemlabels = do
  initGUI
  putStrLn "Open Gaussianity Monitors Window"

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
    let gaussianityActiveLabels = getActiveLabels gaussianityChannelCButtons
    case length gaussianityActiveLabels of 0 -> hasKalGuiMessage "Error" "Not selected channels"
                                           _ -> hasKalGuiRayleighMon gaussianityActiveLabels
    widgetDestroy gaussianityWindow
    hasKalGuiGaussianity activeSubSystemlabels
  {--  Select Closed  --}
  onClicked gaussianityCloseButton $ do
    putStrLn "Closed Gaussianity Monitors Window"
    widgetDestroy gaussianityWindow

  {--  Exit Process  --}
  onDestroy gaussianityWindow mainQuit
  widgetShowAll gaussianityWindow
  mainGUI



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
    HPPR.hroot_core (map fromIntegral [0,1..(rmStride `div` 2 {-+ 1-})]) ( (transposed $ HMRRM.rayleighMon rmStride rmStride rmSampling (map realToFrac (HFF.eval frData))) !! 0) "frequency [Hz]" "noise level [/rHz]" HPPOR.LogXY HPPOR.Line "X11"
    -- 横軸の値を直す(1秒スペクトルなので今は正しい)

    {-- 暫定的なファイル出力 --}
    oFile <- SIO.openFile "./GUI-RayleighMon_Result.txt" SIO.WriteMode
    SIO.hPutStrLn oFile (convert_DLL2S $ HMRRM.rayleighMon rmStride rmStride rmSampling (map realToFrac (HFF.eval frData)) )
    SIO.hClose oFile
    {--  ここまで、ファイル出力  --}
    --widgetDestroy rayleighMonWindow


  {--  Exit Process  --}
  onDestroy rayleighMonWindow mainQuit
  widgetShowAll rayleighMonWindow
  mainGUI


{-- Range Monitor Window
-- test code
main = IO ()
main = hasKalGuiRangeMon
-- arguments: Nothing
--}
hasKalGuiRangeMon :: IO ()
hasKalGuiRangeMon = do
  initGUI
  putStrLn "Open RangeMon Window"

  {-- Create new object --}
  rangeMonWindow <- windowNew
  rangeMonVBox <- vBoxNew True 10
  rangeMonVBox2 <- vBoxNew True 10

  {-- Information --}
  let rangeMonLabels = ["Inspiral", "Ringdown"]
  
  rangeMonButtons <- mapM buttonNewWithLabel rangeMonLabels
  rangeMonCloseButton <- buttonNewWithLabel "Close"

  {--  Set Parameters of the objects  --}
  set rangeMonWindow [ windowTitle := "Range Monitor",
                       windowDefaultWidth := 200,
                       windowDefaultHeight := 300,
                       containerChild := rangeMonVBox,
                       containerBorderWidth := 20 ]

  {--  Arrange object in window  --}
  boxPackStartDefaults rangeMonVBox rangeMonVBox2
  mapM (boxPackStartDefaults rangeMonVBox2) rangeMonButtons
  boxPackStartDefaults rangeMonVBox2 rangeMonCloseButton

  {--  Select Range Monitor  --}
  onClicked (rangeMonButtons !! 0) $ do
    hasKalGuiInspiralRange
  onClicked (rangeMonButtons !! 1) $ do
    hasKalGuiRingDownRange
  onClicked rangeMonCloseButton $ do
    putStrLn "Closed RangeMon Window"
    widgetDestroy rangeMonWindow

  {--  Exit Process  --}
  onDestroy rangeMonWindow mainQuit
  widgetShowAll rangeMonWindow
  mainGUI
   


{--  Inspiral Range Window
-- test code
main = IO ()
main = hasKalGuiInspiralRange
-- arguments: Nothing
--}
hasKalGuiInspiralRange :: IO ()
hasKalGuiInspiralRange = do
  initGUI
  putStrLn "Open InspiralRange Window"

  {--  Create new object  --}
  inspiralRangeWindow <- windowNew
  inspiralRangeVBox <- vBoxNew True 5
  inspiralRangeHBoxYear <- hBoxNew True 5
  inspiralRangeHBoxMonth <- hBoxNew True 5
  inspiralRangeHBoxDay <- hBoxNew True 5
  inspiralRangeHBoxHour <- hBoxNew True 5
  inspiralRangeHBoxMinute <- hBoxNew True 5
  inspiralRangeHBoxSecond <- hBoxNew True 5
  inspiralRangeHBoxObsTime <- hBoxNew True 5
  inspiralRangeHBoxMass1 <- hBoxNew True 5
  inspiralRangeHBoxMass2 <- hBoxNew True 5
  inspiralRangeHBoxThreshold <- hBoxNew True 5
  inspiralRangeHBoxButtons <- hBoxNew True 5

  inspiralRangeYearEntryLabel <- labelNewWithMnemonic "Year"
  inspiralRangeMonthEntryLabel <- labelNewWithMnemonic "Month"
  inspiralRangeDayEntryLabel <- labelNewWithMnemonic "Day"
  inspiralRangeHourEntryLabel <- labelNewWithMnemonic "Hour"
  inspiralRangeMinuteEntryLabel <- labelNewWithMnemonic "Minute"
  inspiralRangeSecondEntryLabel <- labelNewWithMnemonic "Second (JST)"
  inspiralRangeObsTimeEntryLabel <- labelNewWithMnemonic "OBS Time [s]"
  inspiralRangeMass1EntryLabel <- labelNewWithMnemonic "mass_min [M_solar]"
  inspiralRangeMass2EntryLabel <- labelNewWithMnemonic "mass_max [M_solar]"
  inspiralRangeThresholdEntryLabel <- labelNewWithMnemonic "Threshold SNR"

  inspiralRangeYearEntry <- entryNew
  inspiralRangeMonthEntry <- entryNew
  inspiralRangeDayEntry <- entryNew
  inspiralRangeHourEntry <- entryNew
  inspiralRangeMinuteEntry <- entryNew
  inspiralRangeSecondEntry <- entryNew
  inspiralRangeObsTimeEntry <- entryNew
  inspiralRangeMass1Entry <- entryNew
  inspiralRangeMass2Entry <- entryNew
  inspiralRangeThresholdEntry <- entryNew

  inspiralRangeClose <- buttonNewWithLabel "Close"
  inspiralRangeExecute <- buttonNewWithLabel "Execute"

  entrySetText inspiralRangeYearEntry "2013"
  entrySetText inspiralRangeMonthEntry "10"
  entrySetText inspiralRangeDayEntry "21"
  entrySetText inspiralRangeHourEntry "21"
  entrySetText inspiralRangeMinuteEntry "0"
  entrySetText inspiralRangeSecondEntry "0"
  entrySetText inspiralRangeObsTimeEntry "300"
  entrySetText inspiralRangeMinuteEntry "0"
  entrySetText inspiralRangeSecondEntry "0"
  entrySetText inspiralRangeObsTimeEntry "300"
  entrySetText inspiralRangeMass1Entry "1.0"
  entrySetText inspiralRangeMass2Entry "300.0"
  entrySetText inspiralRangeThresholdEntry "10.0"
  
  {--  Set Parameters of the objects  --}
  set inspiralRangeWindow [ windowTitle := "Inspiral Range",
                      windowDefaultWidth := 200,
                      windowDefaultHeight := 450,
                      containerChild := inspiralRangeVBox,
                      containerBorderWidth := 20 ]

  {--  Arrange object in window  --}
  boxPackStartDefaults inspiralRangeVBox inspiralRangeHBoxYear
  boxPackStartDefaults inspiralRangeHBoxYear inspiralRangeYearEntryLabel
  boxPackStartDefaults inspiralRangeHBoxYear inspiralRangeYearEntry
  boxPackStartDefaults inspiralRangeVBox inspiralRangeHBoxMonth
  boxPackStartDefaults inspiralRangeHBoxMonth inspiralRangeMonthEntryLabel
  boxPackStartDefaults inspiralRangeHBoxMonth inspiralRangeMonthEntry
  boxPackStartDefaults inspiralRangeVBox inspiralRangeHBoxDay
  boxPackStartDefaults inspiralRangeHBoxDay inspiralRangeDayEntryLabel
  boxPackStartDefaults inspiralRangeHBoxDay inspiralRangeDayEntry
  boxPackStartDefaults inspiralRangeVBox inspiralRangeHBoxHour
  boxPackStartDefaults inspiralRangeHBoxHour inspiralRangeHourEntryLabel
  boxPackStartDefaults inspiralRangeHBoxHour inspiralRangeHourEntry
  boxPackStartDefaults inspiralRangeVBox inspiralRangeHBoxMinute
  boxPackStartDefaults inspiralRangeHBoxMinute inspiralRangeMinuteEntryLabel
  boxPackStartDefaults inspiralRangeHBoxMinute inspiralRangeMinuteEntry
  boxPackStartDefaults inspiralRangeVBox inspiralRangeHBoxSecond
  boxPackStartDefaults inspiralRangeHBoxSecond inspiralRangeSecondEntryLabel
  boxPackStartDefaults inspiralRangeHBoxSecond inspiralRangeSecondEntry

  boxPackStartDefaults inspiralRangeVBox inspiralRangeHBoxObsTime
  boxPackStartDefaults inspiralRangeHBoxObsTime inspiralRangeObsTimeEntryLabel
  boxPackStartDefaults inspiralRangeHBoxObsTime inspiralRangeObsTimeEntry
  boxPackStartDefaults inspiralRangeVBox inspiralRangeHBoxMass1
  boxPackStartDefaults inspiralRangeHBoxMass1 inspiralRangeMass1EntryLabel
  boxPackStartDefaults inspiralRangeHBoxMass1 inspiralRangeMass1Entry
  boxPackStartDefaults inspiralRangeVBox inspiralRangeHBoxMass2
  boxPackStartDefaults inspiralRangeHBoxMass2 inspiralRangeMass2EntryLabel
  boxPackStartDefaults inspiralRangeHBoxMass2 inspiralRangeMass2Entry
  boxPackStartDefaults inspiralRangeVBox inspiralRangeHBoxThreshold
  boxPackStartDefaults inspiralRangeHBoxThreshold inspiralRangeThresholdEntryLabel
  boxPackStartDefaults inspiralRangeHBoxThreshold inspiralRangeThresholdEntry

  boxPackStartDefaults inspiralRangeVBox inspiralRangeHBoxButtons
  boxPackStartDefaults inspiralRangeHBoxButtons inspiralRangeClose
  boxPackStartDefaults inspiralRangeHBoxButtons inspiralRangeExecute

  {--  Execute  --}
  onClicked inspiralRangeClose $ do
    putStrLn "Closed InspiralRange Window"
    widgetDestroy inspiralRangeWindow
  onClicked inspiralRangeExecute $ do
    putStrLn "Execute"

    s_temp <- entryGetText inspiralRangeYearEntry
    let inspYear = read s_temp :: Int
    s_temp <- entryGetText inspiralRangeMonthEntry
    let inspMonth = read s_temp :: Int
    s_temp <- entryGetText inspiralRangeDayEntry
    let inspDay = read s_temp :: Int
    s_temp <- entryGetText inspiralRangeHourEntry
    let inspHour = read s_temp :: Int
    s_temp <- entryGetText inspiralRangeMinuteEntry
    let inspMinute = read s_temp :: Int
    s_temp <- entryGetText inspiralRangeSecondEntry
    let inspSecond = read s_temp :: Int

    let inspiralRangeDateStr = iDate2sDate inspYear inspMonth inspDay inspHour inspMinute inspSecond
    putStrLn ("   JST Time: " ++ inspiralRangeDateStr)
    let hogeGPS = HTG.time2gps inspiralRangeDateStr
    putStrLn ("   GPS Time: " ++ hogeGPS)
    s_temp <- entryGetText inspiralRangeObsTimeEntry
    let inspObsTime = read s_temp :: Int
    putStrLn ("   Obs Time: " ++ (show inspObsTime) )
    s_temp <- entryGetText inspiralRangeMass1Entry
    let inspMass1 = read s_temp :: Double
    putStrLn ("     Mass_1: " ++ (show inspMass1) )
    s_temp <- entryGetText inspiralRangeMass2Entry
    let inspMass2 = read s_temp :: Double
    putStrLn ("     Mass_2: " ++ (show inspMass2) )
    s_temp <- entryGetText inspiralRangeThresholdEntry
    let inspThreshold = read s_temp :: Double
    putStrLn ("   Thresold: " ++ (show inspThreshold) )
    {-- Monitor tool --}
    -- 複数要素のvectorを与えるとおかしいのでとりあえずforMで代用
    inspDist <- CM.forM [inspMass1, inspMass1+2..inspMass2] $ \mass ->
      return $ NLA.toList $ HMRIRD.distInspiral (NLA.fromList [mass]) (NLA.fromList [mass]) (NLA.fromList [inspThreshold]) HDD.KAGRA
    HPPR.hroot_core (map (*2) [inspMass1,inspMass1+2..inspMass2]) (concat inspDist) "Total Mass [M_sol]" "Distance [Mpc]" HPPOR.LogXY HPPOR.Line "X11"
    {-- End of Monitor Tool --}
    -- putStrLn "Closed InspiralRange Window"
    -- widgetDestroy inspiralRangeWindow

  {--  Exit Process  --}
  onDestroy inspiralRangeWindow mainQuit
  widgetShowAll inspiralRangeWindow
  mainGUI




{--  RingDown Range Window
-- test code
main = IO ()
main = hasKalGuiRingDownRange
-- arguments: Nothing
--}
hasKalGuiRingDownRange :: IO ()
hasKalGuiRingDownRange = do
  initGUI
  putStrLn "Open RingDownRange Window"

  {--  Create new object  --}
  ringDownRangeWindow <- windowNew
  ringDownRangeVBox <- vBoxNew True 5
  ringDownRangeHBoxYear <- hBoxNew True 5
  ringDownRangeHBoxMonth <- hBoxNew True 5
  ringDownRangeHBoxDay <- hBoxNew True 5
  ringDownRangeHBoxHour <- hBoxNew True 5
  ringDownRangeHBoxMinute <- hBoxNew True 5
  ringDownRangeHBoxSecond <- hBoxNew True 5
  ringDownRangeHBoxObsTime <- hBoxNew True 5
  ringDownRangeHBoxMass <- hBoxNew True 5
  ringDownRangeHBoxKerrParam <- hBoxNew True 5
  ringDownRangeHBoxMassDefect <- hBoxNew True 5
  ringDownRangeHBoxIniPhase <- hBoxNew True 5
  ringDownRangeHBoxThreshold <- hBoxNew True 5
  ringDownRangeHBoxButtons <- hBoxNew True 5

  ringDownRangeYearEntryLabel <- labelNewWithMnemonic "Year"
  ringDownRangeMonthEntryLabel <- labelNewWithMnemonic "Month"
  ringDownRangeDayEntryLabel <- labelNewWithMnemonic "Day"
  ringDownRangeHourEntryLabel <- labelNewWithMnemonic "Hour"
  ringDownRangeMinuteEntryLabel <- labelNewWithMnemonic "Minute"
  ringDownRangeSecondEntryLabel <- labelNewWithMnemonic "Second (JST)"
  ringDownRangeObsTimeEntryLabel <- labelNewWithMnemonic "OBS Time [s]"
  ringDownRangeMassEntryLabel <- labelNewWithMnemonic "mass [M_solar]"
  ringDownRangeKerrParamEntryLabel <- labelNewWithMnemonic "Kerr parameter"
  ringDownRangeMassDefectEntryLabel <- labelNewWithMnemonic "mass defect ratio"
  ringDownRangeIniPhaseEntryLabel <- labelNewWithMnemonic "Initial Phase [rad]"
  ringDownRangeThresholdEntryLabel <- labelNewWithMnemonic "Threshold SNR"

  ringDownRangeYearEntry <- entryNew
  ringDownRangeMonthEntry <- entryNew
  ringDownRangeDayEntry <- entryNew
  ringDownRangeHourEntry <- entryNew
  ringDownRangeMinuteEntry <- entryNew
  ringDownRangeSecondEntry <- entryNew
  ringDownRangeObsTimeEntry <- entryNew
  ringDownRangeMassEntry <- entryNew
  ringDownRangeKerrParamEntry <- entryNew
  ringDownRangeMassDefectEntry <- entryNew
  ringDownRangeIniPhaseEntry <- entryNew
  ringDownRangeThresholdEntry <- entryNew

  ringDownRangeClose <- buttonNewWithLabel "Close"
  ringDownRangeExecute <- buttonNewWithLabel "Execute"

  entrySetText ringDownRangeYearEntry "2013"
  entrySetText ringDownRangeMonthEntry "10"
  entrySetText ringDownRangeDayEntry "21"
  entrySetText ringDownRangeHourEntry "21"
  entrySetText ringDownRangeMinuteEntry "0"
  entrySetText ringDownRangeSecondEntry "0"
  entrySetText ringDownRangeObsTimeEntry "300"
  entrySetText ringDownRangeMinuteEntry "0"
  entrySetText ringDownRangeSecondEntry "0"
  entrySetText ringDownRangeObsTimeEntry "300"
  entrySetText ringDownRangeMassEntry "10.0"
  entrySetText ringDownRangeKerrParamEntry "0.98"
  entrySetText ringDownRangeMassDefectEntry "0.03"
  entrySetText ringDownRangeIniPhaseEntry "0.0"
  entrySetText ringDownRangeThresholdEntry "10.0"
  
  {--  Set Parameters of the objects  --}
  set ringDownRangeWindow [ windowTitle := "RingDown Range",
                      windowDefaultWidth := 200,
                      windowDefaultHeight := 450,
                      containerChild := ringDownRangeVBox,
                      containerBorderWidth := 20 ]

  {--  Arrange object in window  --}
  boxPackStartDefaults ringDownRangeVBox ringDownRangeHBoxYear
  boxPackStartDefaults ringDownRangeHBoxYear ringDownRangeYearEntryLabel
  boxPackStartDefaults ringDownRangeHBoxYear ringDownRangeYearEntry
  boxPackStartDefaults ringDownRangeVBox ringDownRangeHBoxMonth
  boxPackStartDefaults ringDownRangeHBoxMonth ringDownRangeMonthEntryLabel
  boxPackStartDefaults ringDownRangeHBoxMonth ringDownRangeMonthEntry
  boxPackStartDefaults ringDownRangeVBox ringDownRangeHBoxDay
  boxPackStartDefaults ringDownRangeHBoxDay ringDownRangeDayEntryLabel
  boxPackStartDefaults ringDownRangeHBoxDay ringDownRangeDayEntry
  boxPackStartDefaults ringDownRangeVBox ringDownRangeHBoxHour
  boxPackStartDefaults ringDownRangeHBoxHour ringDownRangeHourEntryLabel
  boxPackStartDefaults ringDownRangeHBoxHour ringDownRangeHourEntry
  boxPackStartDefaults ringDownRangeVBox ringDownRangeHBoxMinute
  boxPackStartDefaults ringDownRangeHBoxMinute ringDownRangeMinuteEntryLabel
  boxPackStartDefaults ringDownRangeHBoxMinute ringDownRangeMinuteEntry
  boxPackStartDefaults ringDownRangeVBox ringDownRangeHBoxSecond
  boxPackStartDefaults ringDownRangeHBoxSecond ringDownRangeSecondEntryLabel
  boxPackStartDefaults ringDownRangeHBoxSecond ringDownRangeSecondEntry

  boxPackStartDefaults ringDownRangeVBox ringDownRangeHBoxObsTime
  boxPackStartDefaults ringDownRangeHBoxObsTime ringDownRangeObsTimeEntryLabel
  boxPackStartDefaults ringDownRangeHBoxObsTime ringDownRangeObsTimeEntry
  boxPackStartDefaults ringDownRangeVBox ringDownRangeHBoxMass
  boxPackStartDefaults ringDownRangeHBoxMass ringDownRangeMassEntryLabel
  boxPackStartDefaults ringDownRangeHBoxMass ringDownRangeMassEntry
  boxPackStartDefaults ringDownRangeVBox ringDownRangeHBoxKerrParam
  boxPackStartDefaults ringDownRangeHBoxKerrParam ringDownRangeKerrParamEntryLabel
  boxPackStartDefaults ringDownRangeHBoxKerrParam ringDownRangeKerrParamEntry
  boxPackStartDefaults ringDownRangeVBox ringDownRangeHBoxMassDefect
  boxPackStartDefaults ringDownRangeHBoxMassDefect ringDownRangeMassDefectEntryLabel
  boxPackStartDefaults ringDownRangeHBoxMassDefect ringDownRangeMassDefectEntry
  boxPackStartDefaults ringDownRangeVBox ringDownRangeHBoxIniPhase
  boxPackStartDefaults ringDownRangeHBoxIniPhase ringDownRangeIniPhaseEntryLabel
  boxPackStartDefaults ringDownRangeHBoxIniPhase ringDownRangeIniPhaseEntry
  boxPackStartDefaults ringDownRangeVBox ringDownRangeHBoxThreshold
  boxPackStartDefaults ringDownRangeHBoxThreshold ringDownRangeThresholdEntryLabel
  boxPackStartDefaults ringDownRangeHBoxThreshold ringDownRangeThresholdEntry
  boxPackStartDefaults ringDownRangeVBox ringDownRangeHBoxButtons
  boxPackStartDefaults ringDownRangeHBoxButtons ringDownRangeClose
  boxPackStartDefaults ringDownRangeHBoxButtons ringDownRangeExecute

  {--  Execute  --}
  onClicked ringDownRangeClose $ do
    putStrLn "Closed RingDownRange Window"
    widgetDestroy ringDownRangeWindow
  onClicked ringDownRangeExecute $ do
    putStrLn "Execute"

    s_temp <- entryGetText ringDownRangeYearEntry
    let ringDYear = read s_temp :: Int
    s_temp <- entryGetText ringDownRangeMonthEntry
    let ringDMonth = read s_temp :: Int
    s_temp <- entryGetText ringDownRangeDayEntry
    let ringDDay = read s_temp :: Int
    s_temp <- entryGetText ringDownRangeHourEntry
    let ringDHour = read s_temp :: Int
    s_temp <- entryGetText ringDownRangeMinuteEntry
    let ringDMinute = read s_temp :: Int
    s_temp <- entryGetText ringDownRangeSecondEntry
    let ringDSecond = read s_temp :: Int

    let ringDownRangeDateStr = iDate2sDate ringDYear ringDMonth ringDDay ringDHour ringDMinute ringDSecond
    putStrLn ("   JST Time: " ++ ringDownRangeDateStr)
    let hogeGPS = HTG.time2gps ringDownRangeDateStr
    putStrLn ("   GPS Time: " ++ hogeGPS)
    s_temp <- entryGetText ringDownRangeObsTimeEntry
    let ringDObsTime = read s_temp :: Int
    putStrLn ("   Obs Time: " ++ (show ringDObsTime) )
    s_temp <- entryGetText ringDownRangeMassEntry
    let ringDMass = read s_temp :: Double
    putStrLn ("       Mass: " ++ (show ringDMass) )
    s_temp <- entryGetText ringDownRangeKerrParamEntry
    let ringDKerrParam = read s_temp :: Double
    putStrLn (" Kerr Param: " ++ (show ringDKerrParam) )
    s_temp <- entryGetText ringDownRangeMassDefectEntry
    let ringDMassDefect = read s_temp :: Double
    putStrLn ("Mass defect: " ++ (show ringDMassDefect) )
    s_temp <- entryGetText ringDownRangeIniPhaseEntry
    let ringDIniPhase = read s_temp :: Double
    putStrLn ("  Ini Phase: " ++ (show ringDIniPhase) )
    s_temp <- entryGetText ringDownRangeThresholdEntry
    let ringDThreshold = read s_temp :: Double
    putStrLn ("   Thresold: " ++ (show ringDThreshold) )
    {-- Monitor tool --}
    ringDDist <- CM.forM [1.0*ringDMass, 10.0*ringDMass..300.0*ringDMass] $ \mass -> 
      return $ NLA.toList $ HMRIRD.distRingdown (NLA.fromList [mass]) (NLA.fromList [ringDThreshold]) (NLA.fromList [ringDKerrParam]) (NLA.fromList [ringDMassDefect]) (NLA.fromList [ringDIniPhase]) HDD.KAGRA
    HPPR.hroot_core [1.0*ringDMass, 10.0*ringDMass..300.0*ringDMass] (concat ringDDist) "mass [M_sol]" "Distance [Mpc]" HPPOR.LogXY HPPOR.Line "X11"
   {-- End of Monitor Tool --}
    -- putStrLn "Closed RingDownRange Window"
    --widgetDestroy ringDownRangeWindow


  {--  Exit Process  --}
  onDestroy ringDownRangeWindow mainQuit
  widgetShowAll ringDownRangeWindow
  mainGUI



{-- Message window
-- test code
main :: IO ()
main = hasKalGuiMessage "error orz"
--}
hasKalGuiMessage :: String -> String -> IO ()
hasKalGuiMessage messageTitle messageSentence = do
  initGUI
  putStrLn "Open Message Window"

  messageWindow <- windowNew
  messageSentenceVBox <- vBoxNew True 10
  messageCloseVBox <- vBoxNew True 10
  messageCloseButton <- buttonNewWithLabel "Close"

  messageTitleLabel <- labelNewWithMnemonic messageTitle
  messageSentenceLabel <- labelNewWithMnemonic messageSentence

  set messageWindow [ windowTitle := "Message window",
                    windowDefaultWidth := 200,
                    windowDefaultHeight := 150,
                    containerChild := messageSentenceVBox,
                    containerBorderWidth := 20 ]

  boxPackStartDefaults messageSentenceVBox messageTitleLabel
  boxPackStartDefaults messageSentenceVBox messageSentenceLabel
  boxPackStartDefaults messageSentenceVBox messageCloseButton

  {--  Execute  --}
  onClicked messageCloseButton $ do
    putStrLn "Closed Message Window"
    widgetDestroy messageWindow

  {--  Exit Process  --}
  onDestroy messageWindow mainQuit
  widgetShowAll messageWindow
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
-- transposed :: [[Double]] -> [[Double]]
-- transposed xxs = [ concat $ map ((drop (m-1)).(take m)) xxs | m <- [1..(length (head xxs))] ]
transposed :: [[a]] -> [[a]]
transposed [] = []
transposed ([] : xss) = transposed xss
transposed ((x:xs) : xss) = (x : [y | (y:_) <- xss]) : transposed (xs : [z | (_:z) <- xss])
-- [ [h_1(f=0), h_1(f=1), ..], [h_2(f=0), h_2(f=1), ..], ..] -> [ [h_1(f=0), h_2(f=0), ..], [h_1(f=1), h_2(f=1), ..], ..]
-- same as in RayleighMon

