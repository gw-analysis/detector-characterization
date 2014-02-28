{-******************************************************************
  *     File Name: GUI_Utils.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/02/28 15:22:09
  ******************************************************************-}
--test
module HasKAL.GUI_Utils.GUI_Utils
  (hasKalGuiTop
   --,hasKalGuiGlitch
  ) where

import Graphics.UI.Gtk
import System.IO -- openFile
import Control.Monad -- forM
import Text.Regex -- splitRegex, mkRegex

import HasKAL.MonitorUtils.EXTKleineWelle as Monitor
import HasKAL.PlotUtils.PlotUtils as Plot


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
    putStr =<< buttonGetLabel (topMonitorButtons !! 0)
    putStrLn " Monitor: New window open."

    topSubSystemFlag <- mapM toggleButtonGetActive topSubSystemCheckButtons
    let topActiveNum = [idxNum | idxNum <- [0..topNumOfSubSystems-1], (topSubSystemFlag !! idxNum) == True]
    topActiveLabels <- forM topActiveNum $ \lambda -> do
      topActiveLabel <- buttonGetLabel (topSubSystemCheckButtons !! lambda)
      return topActiveLabel

    hasKalGuiGlitch topActiveLabels
  {--  Select Line Monitor --}
  onClicked (topMonitorButtons !! 1) $ do
    putStr =<< buttonGetLabel (topMonitorButtons !! 1)
    putStrLn " Monitor: Not implemented yet." 
  {--  Select Gaussianity Monitor --}
  onClicked (topMonitorButtons !! 2) $ do
    putStr =<< buttonGetLabel (topMonitorButtons !! 2)
    putStrLn " Monitor: Not implemented yet." 
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
  let kwUnowen_1 = 16
  let kwUnowen_2 = 2
  let kwOptFilePref = "optKW_"
  let kwListFile = "gwffilelist.txt"

  {--  Read file of channel list  --}
  let glitchNumOfSubSystems = length activeSubSystemlabels
  glitchChannels <- forM [0..glitchNumOfSubSystems-1] $ \lambda -> do
    let glitchFilename = "./ChList/channelList" ++ (activeSubSystemlabels !! lambda)
    glitchIFile <- openFile (glitchFilename ++ ".txt") ReadMode
    glitchChannel <- hGetContents glitchIFile
    return glitchChannel

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
  glitchVBox <- vBoxNew True 10
  glitchVBox2 <- vBoxNew True 10
  glitchHBox0 <- hBoxNew True 10
  glitchHBox <- hBoxNew True 10
  glitchHBox2 <- hBoxNew True 10
  glitchHBox3 <- hBoxNew True 10
  glitchHBox4 <- hBoxNew True 10
  glitchHBox5 <- hBoxNew True 10
  glitchHBox6 <- hBoxNew True 10

  glitchChannelScroll <- scrolledWindowNew Nothing Nothing
  glitchChannelBBox <- vButtonBoxNew
  glitchChannelCButtons <- mapM checkButtonNewWithLabel glitchChannelLabels
  glitchGpsEntryLable <- labelNewWithMnemonic "GPS Time [s]"
  glitchObsEntryLable <- labelNewWithMnemonic "OBS Time [s]"
  glitchStrideEntryLable <- labelNewWithMnemonic "Stride"
  glitchSignificanceEntryLable <- labelNewWithMnemonic "Significance"
  glitchThresholdEntryLable <- labelNewWithMnemonic "Threshold"
  glitchFsampleEntryLable <- labelNewWithMnemonic "Fsample"
  glitchGpsEntry <- entryNew
  glitchObsEntry <- entryNew
  glitchStrideEntry <- entryNew
  glitchSignificanceEntry <- entryNew
  glitchThresholdEntry <- entryNew
  glitchFsampleEntry <- entryNew
  glitchClose <- buttonNewWithLabel "Close"
  glitchExecute <- buttonNewWithLabel "Execute"
  entrySetText glitchGpsEntry "970014976"
  entrySetText glitchObsEntry "128"
  entrySetText glitchStrideEntry "16"
  entrySetText glitchSignificanceEntry "2.0"
  entrySetText glitchThresholdEntry "3.0"
  entrySetText glitchFsampleEntry "4096"

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
  boxPackStartDefaults glitchVBox2 glitchHBox
  boxPackStartDefaults glitchHBox glitchGpsEntryLable
  boxPackStartDefaults glitchHBox glitchGpsEntry
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
  boxPackStartDefaults glitchHBox6 glitchFsampleEntryLable
  boxPackStartDefaults glitchHBox6 glitchFsampleEntry

  mapM (boxPackStartDefaults kwChannelBBox) kwChannelCButtons
  scrolledWindowAddWithViewport kwChannelScroll kwChannelBBox
  boxPackStartDefaults glitchVBox kwChannelScroll

  boxPackStartDefaults glitchVBox2 glitchHBox0
  boxPackStartDefaults glitchHBox0 glitchClose
  boxPackStartDefaults glitchHBox0 glitchExecute


  {--  Execute --}
  onClicked glitchClose $ do
    putStrLn "Close Glitch Monitor\n"
    widgetDestroy glitchWindow
  onClicked glitchExecute $ do
    putStrLn "Execute"
    glitchChannelFlag <- mapM toggleButtonGetActive glitchChannelCButtons
    let glitchActiveNum = [idxNum | idxNum <- [0..glitchNumOfChannel-1], (glitchChannelFlag !! idxNum) == True]
    glitchActiveLabels <- forM glitchActiveNum $ \lambda -> do
      glitchActiveLabel <- buttonGetLabel (glitchChannelCButtons !! lambda)
      return glitchActiveLabel

    kwChannelFlag <- mapM toggleButtonGetActive kwChannelCButtons
    let kwActiveNum = [idxNum | idxNum <- [0..kwNumOfChannel-1], (kwChannelFlag !! idxNum) == True]
    kwActiveLabels <- forM kwActiveNum $ \lambda -> do
      kwActiveLabel <- buttonGetLabel (kwChannelCButtons !! lambda)
      return kwActiveLabel

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
    s_temp <- entryGetText glitchFsampleEntry
    let kwFsample = read s_temp :: Int
    putStrLn ("   Fsample: " ++ (show kwFsample) )
    putStrLn "   Channels: "
    mapM_ putStrLn glitchActiveLabels
    putStrLn "   Column: "
    mapM_ putStrLn kwActiveLabels
    putStrLn "Generate optM file for KleineWell"
    lwtOutput <- Monitor.execKleineWelle kwStride kwBasename kwTransientDuration kwSignificance kwThreshold kwDecimateFactor glitchActiveLabels kwUnowen_1 kwFsample kwUnowen_2 kwOptFilePref kwListFile kwGpsTime kwActiveLabels
    let lwtColmunNum = length kwActiveLabels
    putStrLn "Run Plot tool"
    if lwtColmunNum == 2
      then forM [0..(length glitchActiveLabels)-1] $ \lambda -> do
             Plot.scatter_plot_2d "TITLE 2" "HOGEHOGE" 10.0 (640,480) (convert_StoDT2L (lwtOutput !! lambda))
      else if lwtColmunNum == 3
        then forM [0..(length glitchActiveLabels)-1] $ \lambda -> do
               Plot.scatter_plot_3d "TITLE 3" "HOGEHOGE" 10.0 (640,480) (convert_StoDT3L (lwtOutput !! lambda))
        else forM [0] $ \lambda -> do
               putStrLn "Required 2 or 3 columns\n"
    putStrLn "Close Glitch Monitor\n"
    widgetDestroy glitchWindow
  {--  Exit Process  --}
  onDestroy glitchWindow mainQuit
  widgetShowAll glitchWindow
  mainGUI


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

