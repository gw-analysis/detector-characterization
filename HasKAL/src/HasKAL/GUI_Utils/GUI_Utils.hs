{-******************************************************************
  *     File Name: GUI_Utils.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/03/14 18:09:52
  ******************************************************************-}

module HasKAL.GUI_Utils.GUI_Utils
  (hasKalGuiTop
   --,hasKalGuiGlitch
  ) where

import Graphics.UI.Gtk
import System.IO -- openFile
import Control.Monad -- forM
import Text.Regex -- splitRegex, mkRegex
import Text.Printf -- printf

import System.IO.Unsafe -- unsafePerformIO

import HasKAL.MonitorUtils.EXTKleineWelle as Monitor
import HasKAL.PlotUtils.PlotUtils as Plot
import HasKAL.TimeUtils.GPSfunction as Time

hasKalGuiTop :: IO ()
hasKalGuiTop = do
  initGUI

  {--  information  --}
  let topSubSystemLabels = ["test", "TUN", "FCL", "VAC", "CRY", "VIS", "MIR", "LAS", "MIF", "IOO", "AOS", "AEL", "DGS", "DAS", "GIF", "DC"] -- sub system names (ハードコーディングで良いか？
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
    putStrLn =<< fmap (++ " Monitor: Not implemented yet.") (buttonGetLabel (topMonitorButtons !! 2))
  {--  Select Exit  --}
  onClicked topExitButton $ do
    putStrLn "Exit"
    widgetDestroy topWindow

  {--  Exit Process  --}
  onDestroy topWindow mainQuit
  widgetShowAll topWindow
  mainGUI


-- Called in hasKalGuiStarup
hasKalGuiGlitch :: [String] -> IO ()
hasKalGuiGlitch activeSubSystemlabels = do
  initGUI

  {--  Fixed value for KleineWelle  --}
  let kwBasename = "KW_"
  let kwTransientDuration = 4.0
  let kwDecimateFactor = -1
  let kwUnowen_2 = 2
  let kwOptFilePref = "optKW_"
  let kwListFile = "gwffilelist.txt"

  {--  Read file of channel list  --}
  glitchChannels <- forM activeSubSystemlabels $ \lambda -> hGetContents =<< openFile ("../ChList/channelList" ++ lambda ++ ".txt") ReadMode --glitchIFile

  {--  for lwtprint  --}
  let kwChannelLabels = [ "ifo", "peak_time", "peak_time_ns", "start_time", "start_time_ns", "duration", "search", "central_freq", "channel", "amplitude", "snr", "confidence", "chisq", "chisq_dof", "bandwidth", "event_id", "process_id", "table" ]
  kwChannelScroll <- scrolledWindowNew Nothing Nothing
  kwChannelBBox <- vButtonBoxNew
  kwChannelCButtons <- mapM checkButtonNewWithLabel kwChannelLabels


  {--  Information  --}
  let glitchChannelLabels = lines (concat glitchChannels)
  let glitchNumOfChannel = length glitchChannelLabels
  let kwNumOfChannel = length kwChannelLabels

  {--  Create new object --}
  glitchWindow <- windowNew
  glitchVBox <- vBoxNew True 5
  glitchVBox2 <- vBoxNew True 5
  glitchHBox0 <- hBoxNew True 5
  glitchHBox <- hBoxNew True 5
  glitchHBox00 <- hBoxNew True 5
  glitchHBox1 <- hBoxNew True 5
  glitchHBox11 <- hBoxNew True 5
  glitchHBox12 <- hBoxNew True 5
  glitchHBox13 <- hBoxNew True 5
  glitchHBox2 <- hBoxNew True 5
  glitchHBox3 <- hBoxNew True 5
  glitchHBox4 <- hBoxNew True 5
  glitchHBox5 <- hBoxNew True 5
  glitchHBox6 <- hBoxNew True 5
  glitchHBox7 <- hBoxNew True 5


  glitchChannelScroll <- scrolledWindowNew Nothing Nothing
  glitchChannelBBox <- vButtonBoxNew
  glitchChannelCButtons <- mapM checkButtonNewWithLabel glitchChannelLabels
  glitchGpsEntryLable <- labelNewWithMnemonic "GPS Time [s]"
{--}
  glitchYearEntryLable <- labelNewWithMnemonic "年"
  glitchMonthEntryLable <- labelNewWithMnemonic "月"
  glitchDayEntryLable <- labelNewWithMnemonic "日"
  glitchHourEntryLable <- labelNewWithMnemonic "時"
  glitchMinuteEntryLable <- labelNewWithMnemonic "分"
  glitchSecondEntryLable <- labelNewWithMnemonic "秒 (JST)"
{----}
  glitchObsEntryLable <- labelNewWithMnemonic "OBS Time [s]"
  glitchStrideEntryLable <- labelNewWithMnemonic "Stride"
  glitchSignificanceEntryLable <- labelNewWithMnemonic "Significance"
  glitchThresholdEntryLable <- labelNewWithMnemonic "Threshold"
  glitchLowCutOffEntryLable <- labelNewWithMnemonic "LowCutOff [Hz]"
  glitchHighCutOffEntryLable <- labelNewWithMnemonic "HighCutOff [Hz]"
  glitchGpsEntry <- entryNew
{--}
  glitchYearEntry <- entryNew
  glitchMonthEntry <- entryNew
  glitchDayEntry <- entryNew
  glitchHourEntry <- entryNew
  glitchMinuteEntry <- entryNew
  glitchSecondEntry <- entryNew
{----}
  glitchObsEntry <- entryNew
  glitchStrideEntry <- entryNew
  glitchSignificanceEntry <- entryNew
  glitchThresholdEntry <- entryNew
  glitchLowCutOffEntry <- entryNew
  glitchHighCutOffEntry <- entryNew
  glitchClose <- buttonNewWithLabel "Close"
  glitchExecute <- buttonNewWithLabel "Execute"
  entrySetText glitchGpsEntry "1066392016"
{--}
  entrySetText glitchYearEntry "2013"
  entrySetText glitchMonthEntry "10"
  entrySetText glitchDayEntry "21"
  entrySetText glitchHourEntry "21"
  entrySetText glitchMinuteEntry "0"
  entrySetText glitchSecondEntry "0"
{----}
  entrySetText glitchObsEntry "300"
  entrySetText glitchStrideEntry "16"
  entrySetText glitchSignificanceEntry "2.0"
  entrySetText glitchThresholdEntry "3.0"
  entrySetText glitchLowCutOffEntry "10"
  entrySetText glitchHighCutOffEntry "1000"

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
  boxPackStartDefaults glitchVBox glitchHBox00
  boxPackStartDefaults glitchHBox00 glitchChannelScroll

  mapM (boxPackStartDefaults kwChannelBBox) kwChannelCButtons
  scrolledWindowAddWithViewport kwChannelScroll kwChannelBBox
  boxPackStartDefaults glitchHBox00 kwChannelScroll

  boxPackStartDefaults glitchVBox glitchVBox2
  boxPackStartDefaults glitchVBox2 glitchHBox

  -- boxPackStartDefaults glitchHBox glitchGpsEntryLable
  -- boxPackStartDefaults glitchHBox glitchGpsEntry
{--}
  boxPackStartDefaults glitchVBox2 glitchHBox1
  boxPackStartDefaults glitchHBox1 glitchYearEntryLable
  boxPackStartDefaults glitchHBox1 glitchMonthEntryLable
  boxPackStartDefaults glitchHBox1 glitchDayEntryLable
  boxPackStartDefaults glitchVBox2 glitchHBox11
  boxPackStartDefaults glitchHBox11 glitchYearEntry
  boxPackStartDefaults glitchHBox11 glitchMonthEntry
  boxPackStartDefaults glitchHBox11 glitchDayEntry
  boxPackStartDefaults glitchVBox2 glitchHBox12
  boxPackStartDefaults glitchHBox12 glitchHourEntryLable
  boxPackStartDefaults glitchHBox12 glitchMinuteEntryLable
  boxPackStartDefaults glitchHBox12 glitchSecondEntryLable
  boxPackStartDefaults glitchVBox2 glitchHBox13
  boxPackStartDefaults glitchHBox13 glitchHourEntry
  boxPackStartDefaults glitchHBox13 glitchMinuteEntry
  boxPackStartDefaults glitchHBox13 glitchSecondEntry
{----}
  boxPackStartDefaults glitchVBox2 glitchHBox2
  boxPackStartDefaults glitchHBox2 glitchObsEntryLable
  boxPackStartDefaults glitchHBox2 glitchObsEntry
  boxPackStartDefaults glitchVBox2 glitchHBox3
  boxPackStartDefaults glitchHBox3 glitchStrideEntryLable
  boxPackStartDefaults glitchHBox3 glitchStrideEntry
  boxPackStartDefaults glitchVBox2 glitchHBox4
  boxPackStartDefaults glitchHBox4 glitchSignificanceEntryLable
  boxPackStartDefaults glitchHBox4 glitchSignificanceEntry
  boxPackStartDefaults glitchVBox2 glitchHBox5
  boxPackStartDefaults glitchHBox5 glitchThresholdEntryLable
  boxPackStartDefaults glitchHBox5 glitchThresholdEntry
  boxPackStartDefaults glitchVBox2 glitchHBox6
  boxPackStartDefaults glitchHBox6 glitchLowCutOffEntryLable
  boxPackStartDefaults glitchHBox6 glitchLowCutOffEntry
  boxPackStartDefaults glitchVBox2 glitchHBox7
  boxPackStartDefaults glitchHBox7 glitchHighCutOffEntryLable
  boxPackStartDefaults glitchHBox7 glitchHighCutOffEntry

  boxPackStartDefaults glitchVBox2 glitchHBox0
  boxPackStartDefaults glitchHBox0 glitchClose
  boxPackStartDefaults glitchHBox0 glitchExecute


  {--  Execute --}
  onClicked glitchClose $ do
    putStrLn "Close Glitch Monitor\n"
    widgetDestroy glitchWindow
  onClicked glitchExecute $ do
    putStrLn "Execute"
    let glitchActiveLabels = getActiveLabels glitchChannelCButtons
    let kwActiveLabels = getActiveLabels kwChannelCButtons
{--}
    s_temp <- entryGetText glitchYearEntry
    let kwYear = read s_temp :: Int
    s_temp <- entryGetText glitchMonthEntry
    let kwMonth = read s_temp :: Int
    s_temp <- entryGetText glitchDayEntry
    let kwDay = read s_temp :: Int
    s_temp <- entryGetText glitchHourEntry
    let kwHour = read s_temp :: Int
    s_temp <- entryGetText glitchMinuteEntry
    let kwMinute = read s_temp :: Int
    s_temp <- entryGetText glitchSecondEntry
    let kwSecond = read s_temp :: Int
    putStrLn "hoge"
    let glitchDateStr = i_date2s_date kwYear kwMonth kwDay kwHour kwMinute kwSecond
    putStrLn ("   JST Time: " ++ glitchDateStr)
    let hogeGPS = Time.time2gps glitchDateStr
    putStrLn ("   GPS Time: " ++ hogeGPS)
{----}
    s_temp <- entryGetText glitchGpsEntry
    let kwGpsTime = read s_temp :: Int
    putStrLn ("   GPS Time: " ++ (show kwGpsTime) )
    s_temp <- entryGetText glitchObsEntry
    let kwObsTime = read s_temp :: Int
    putStrLn ("   Obs Time: " ++ (show kwObsTime) )
    s_temp <- entryGetText glitchStrideEntry
    let kwStride = read s_temp :: Int
    putStrLn ("   Stride: " ++ (show kwStride) )
    s_temp <- entryGetText glitchSignificanceEntry
    let kwSignificance = read s_temp :: Double
    putStrLn ("   Significance: " ++ (show kwSignificance) )
    s_temp <- entryGetText glitchThresholdEntry
    let kwThreshold = read s_temp :: Double
    putStrLn ("   Threshold: " ++ (show kwThreshold) )
    s_temp <- entryGetText glitchLowCutOffEntry
    let kwLowCutOff = read s_temp :: Int
    putStrLn ("   LowCutOff: " ++ (show kwLowCutOff) )
    s_temp <- entryGetText glitchHighCutOffEntry
    let kwHighCutOff = read s_temp :: Int
    putStrLn ("   HighCutOff: " ++ (show kwHighCutOff) )
    putStrLn "   Channels: "
    mapM_ putStrLn glitchActiveLabels
    putStrLn "   Column: "
    mapM_ putStrLn kwActiveLabels
    putStrLn "Generate optM file for KleineWell"
    lwtOutput <- Monitor.execKleineWelle kwStride kwBasename kwTransientDuration kwSignificance kwThreshold kwDecimateFactor glitchActiveLabels kwLowCutOff kwHighCutOff kwUnowen_2 kwOptFilePref kwListFile kwGpsTime kwActiveLabels
    let lwtColmunNum = length kwActiveLabels
    putStrLn "Run Plot tool"
    if lwtColmunNum == 2
      then forM lwtOutput $ \lambda -> Plot.scatter_plot_2d "TITLE 2" "HOGEHOGE" 10.0 (640,480) (convert_StoDT2L lambda)
      else if lwtColmunNum == 3
        then forM lwtOutput $ \lambda -> Plot.scatter_plot_3d "TITLE 3" "HOGEHOGE" 10.0 (640,480) (convert_StoDT3L lambda)
        else mapM putStrLn ["Required 2 or 3 columns\n"]
    putStrLn "Close Glitch Monitor\n"
    widgetDestroy glitchWindow
  {--  Exit Process  --}
  onDestroy glitchWindow mainQuit
  widgetShowAll glitchWindow
  mainGUI


getActiveLabels :: [CheckButton] -> [String]
getActiveLabels [] = []
getActiveLabels (x:xs) = 
  case unsafePerformIO (toggleButtonGetActive x) of 
    True -> (unsafePerformIO (buttonGetLabel x)):(getActiveLabels xs)
    False -> getActiveLabels xs


convert_StoD :: String -> Double
convert_StoD = read

convert_LtoT2 :: [Double] -> (Double, Double)
convert_LtoT2 [x, y] = (x, y)

convert_LtoT3 :: [Double] -> (Double, Double, Double)
convert_LtoT3 [x, y, z] = (x, y, z)

convert_StoDT2L :: String -> [(Double, Double)]
convert_StoDT2L stringData = map convert_LtoT2 (map (map convert_StoD) (map (splitRegex (mkRegex ",")) (lines stringData) ) )

convert_StoDT3L :: String -> [(Double, Double, Double)]
convert_StoDT3L stringData = map convert_LtoT3 (map (map convert_StoD) (map (splitRegex (mkRegex ",")) (lines stringData) ) )

i_date2s_date :: Int -> Int -> Int -> Int -> Int -> Int -> String
i_date2s_date kwYear kwMonth kwDay kwHour kwMinute kwSecond =
              (printf "%04d" kwYear :: String) ++ "-" ++ (printf "%02d" kwMonth :: String) ++ "-" ++ (printf "%02d" kwDay :: String) ++ " " ++ (printf "%02d" kwHour :: String) ++ ":" ++ (printf "%02d" kwMinute :: String) ++ ":" ++ (printf "%02d" kwSecond :: String) ++ " JST"

-- fillZero :: Int -> Int -> String
-- fillZero digit num = do
--          let times = digit - (length $ show num)
--          repe
         