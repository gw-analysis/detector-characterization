{-# HADDOCK Markdown #-}
{- |
Module      : HasKAL.GUI_Utils.GUI_Utils
Description : This is documentation tests.
Copyright   : (c) WhoAmI, 2014
License     : ???
Maintainer  : hoge@hoge.com
Stability   : test
Portability : POSIX

HasKAL GUI 

-}

module HasKAL.GUI_Utils.GUI_Utils
  (hasKalGuiTop
  ) where

import Graphics.UI.Gtk
import qualified Control.Monad as CM -- forM
import qualified Numeric.LinearAlgebra as NLA -- data Vector, fromList, toList
import qualified System.IO as SIO -- openFile

import qualified HasKAL.GUI_Utils.GUI_AntennaPattern as HGGA
import qualified HasKAL.GUI_Utils.GUI_GaussianityRayleighMon as HGGGR
import qualified HasKAL.GUI_Utils.GUI_GaussianityStudentRayleighMon as HGGGS
import qualified HasKAL.GUI_Utils.GUI_GlitchKleineWelle as HGGGKW
import qualified HasKAL.GUI_Utils.GUI_RangeInspiral as HGGRI
import qualified HasKAL.GUI_Utils.GUI_RangeIMBH as HGGRIMBH
import qualified HasKAL.GUI_Utils.GUI_RangeRingDown as HGGRRD
import qualified HasKAL.GUI_Utils.GUI_Supplement as HGGS
import qualified HasKAL.GUI_Utils.GUI_RangeStochMon as HGGRS

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
  let topSubSystemLabels = ["TestForKW", "TUN", "FCL", "VAC", "CRY", "VIS", "MIR", "LAS", "MIF", "IOO", "AOS", "AEL", "DGS", "DAS", "GIF", "DC"] -- sub system names
  let topNumOfSubSystems = length topSubSystemLabels -- number of sub systems
  let topMonitorLabels = ["Glitch", "Line", "Gaussianity", "RangeMon", "Temporary"] -- monitor names

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
                      windowDefaultHeight := 500,
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
    let topActiveLabels = HGGS.getActiveLabels topSubSystemCheckButtons
    case length topActiveLabels of 0 -> hasKalGuiMessage "Error" "Not selected subsystem."
                                   _ -> hasKalGuiGlitch topActiveLabels
    widgetDestroy topWindow
    hasKalGuiTop
  {--  Select Line Monitor --}
  onClicked (topMonitorButtons !! 1) $ do
    let topActiveLabels = HGGS.getActiveLabels topSubSystemCheckButtons
    case length topActiveLabels of 0 -> hasKalGuiMessage "Error" "Not selected subsystem."
                                   _ -> hasKalGuiMessage "Error" "Not implemented yet."
    widgetDestroy topWindow
    hasKalGuiTop
  {--  Select Gaussianity Monitor --}
  onClicked (topMonitorButtons !! 2) $ do
    let topActiveLabels = HGGS.getActiveLabels topSubSystemCheckButtons
    case length topActiveLabels of 0 -> hasKalGuiMessage "Error" "Not selected subsystem."
                                   _ -> hasKalGuiGaussianity topActiveLabels
    widgetDestroy topWindow
    hasKalGuiTop
  {--  Select Range Monitor --}
  onClicked (topMonitorButtons !! 3) $ do
    hasKalGuiRangeMon
  {--  Select Temporary --}
  onClicked (topMonitorButtons !! 4) $ do
    hasKalGuiTemporary
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
  glitchChannels <- CM.forM activeSubSystemlabels $ \lambda -> SIO.hGetContents =<< SIO.openFile (HGGS.haskalOpt ++ "/channels/channelList" ++ lambda ++ ".txt") SIO.ReadMode --glitchIFile

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
    let glitchActiveLabels = HGGS.getActiveLabels glitchChannelCButtons
    case length glitchActiveLabels of 0 -> hasKalGuiMessage "Error" "Not selected channels"
                                      _ -> HGGGKW.hasKalGuiKleineWelle glitchActiveLabels
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
  gaussianityChannels <- CM.forM activeSubSystemlabels $ \lambda -> SIO.hGetContents =<< SIO.openFile (HGGS.haskalOpt ++ "/channels/channelList" ++ lambda ++ ".txt") SIO.ReadMode --gaussianityIFile

  {--  Information  --}
  let gaussianityMonitorLabels = ["RayleighMon", "SRMon"]
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
    let gaussianityActiveLabels = HGGS.getActiveLabels gaussianityChannelCButtons
    case length gaussianityActiveLabels of 0 -> hasKalGuiMessage "Error" "Not selected channels"
                                           _ -> HGGGR.hasKalGuiRayleighMon gaussianityActiveLabels
  onClicked (gaussianityMonitorButtons !! 1) $ do
    let gaussianityActiveLabels = HGGS.getActiveLabels gaussianityChannelCButtons
    case length gaussianityActiveLabels of 0 -> hasKalGuiMessage "Error" "Not selected channels"
                                           _ -> HGGGS.hasKalGuiStudentRayleighMon gaussianityActiveLabels
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
  let rangeMonLabels = ["Inspiral", "Ringdown", "Insp-Merge-Ring", "Stochastic"]
  
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
    HGGRI.hasKalGuiInspiralRange
  onClicked (rangeMonButtons !! 1) $ do
    HGGRRD.hasKalGuiRingDownRange
  onClicked (rangeMonButtons !! 2) $ do
    HGGRIMBH.hasKalGuiIMR'Range
  onClicked (rangeMonButtons !! 3) $ do
    HGGRS.hasKalGuiStochMon
  onClicked rangeMonCloseButton $ do
    putStrLn "Closed RangeMon Window"
    widgetDestroy rangeMonWindow

  {--  Exit Process  --}
  onDestroy rangeMonWindow mainQuit
  widgetShowAll rangeMonWindow
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



hasKalGuiTemporary :: IO ()
hasKalGuiTemporary = do
  initGUI
  putStrLn "Open Temporary Window"

  {-- Create new object --}
  tempWindow <- windowNew
  tempVBox <- vBoxNew True 10
  tempVBox2 <- vBoxNew True 10

  {-- Information --}
  let tempLabels = ["Antenna Pattern"]
  
  tempButtons <- mapM buttonNewWithLabel tempLabels
  tempCloseButton <- buttonNewWithLabel "Close"

  {--  Set Parameters of the objects  --}
  set tempWindow [ windowTitle := "Temporary",
                       windowDefaultWidth := 200,
                       windowDefaultHeight := 150,
                       containerChild := tempVBox,
                       containerBorderWidth := 20 ]

  {--  Arrange object in window  --}
  boxPackStartDefaults tempVBox tempVBox2
  mapM (boxPackStartDefaults tempVBox2) tempButtons
  boxPackStartDefaults tempVBox2 tempCloseButton

  {--  Select Range Monitor  --}
  onClicked (tempButtons !! 0) $ do
    HGGA.hasKalGuiAntennaPattern
  onClicked tempCloseButton $ do
    putStrLn "Closed Temporary Window"
    widgetDestroy tempWindow

  {--  Exit Process  --}
  onDestroy tempWindow mainQuit
  widgetShowAll tempWindow
  mainGUI


