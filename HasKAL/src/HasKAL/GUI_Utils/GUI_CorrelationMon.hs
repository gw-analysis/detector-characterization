


module HasKAL.GUI_Utils.GUI_CorrelationMon (
  hasKalGuiCorrMon
) where

import Graphics.UI.Gtk
import Data.Maybe (fromJust)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (liftM, zipWithM)

import qualified HasKAL.GUI_Utils.GUI_Supplement as GS
import qualified HasKAL.TimeUtils.GPSfunction as GPS
import HasKAL.FrameUtils.Function
import HasKAL.PlotUtils.HROOT.PlotGraph

import HasKAL.MonitorUtils.CorrelationMon.CalCorrelation
import HasKAL.MonitorUtils.CorrelationMon.CorrelationMethod

hasKalGuiCorrMon :: [String] -> IO ()
hasKalGuiCorrMon activeChannelLabels = do
  initGUI
  putStrLn "Open CorrelationMon Window"

  {-- Create new object --}
  cWindow <- windowNew
  cVBox <- vBoxNew True 5
  cHBoxCache <- hBoxNew True 5
  cHBoxDate <- mapM (hBoxNew True) $ take 7 [5..]
  cHBoxObsTime <- hBoxNew True 5
  cHBoxButtons <- hBoxNew True 5

  cCacheOpener <- GS.fileOpenButtonNewWithLabelDefault "Cache file" $ GS.haskalOpt ++ "/cachefiles/cliocache.lst"
  cDateCombo <- GS.dateComboNew (2012,10,18,9,25,52,"JST")
  cObsTimeEntry <- GS.entryNewWithLabelDefault "OBS Time [s]" "64"
  cButtons <- mapM buttonNewWithLabel ["Close", "Execute"]

  {-- Set Parameters of the objects --}
  set cWindow [windowTitle := "RayleighMon",
               windowDefaultWidth := 300,
               windowDefaultHeight := 450,
               containerChild := cVBox,
               containerBorderWidth := 20 ]

  {-- Arrange object in window --}
  boxPackStart cVBox cHBoxCache PackGrow 0
  GS.boxPackStartDefaultsPair cHBoxCache cCacheOpener
  mapM (\x -> boxPackStart cVBox x PackGrow 0) cHBoxDate
  zipWithM GS.boxPackStartDefaultsPair cHBoxDate cDateCombo
  boxPackStart cVBox cHBoxObsTime PackGrow 0
  GS.boxPackStartDefaultsPair cHBoxObsTime cObsTimeEntry
  boxPackStart cVBox cHBoxButtons PackGrow 0
  mapM (\x -> boxPackStart cHBoxButtons x PackGrow 0) cButtons

  {-- Execute --}
  onClicked (cButtons !! 0) $ do
    putStrLn "Closed CorrelationMon Window"
    widgetDestroy cWindow
  onClicked (cButtons !! 1) $ do
    putStrLn "Execute"
    let cCache = fromJust $ unsafePerformIO $ fileChooserGetFilename $ snd cCacheOpener
    cDate <- liftM GS.dateStr2Tuple $ mapM GS.comboBoxGetActiveString cDateCombo
    let cGPS = read $ GPS.timetuple2gps cDate :: Integer
        cObsTime = read $ unsafePerformIO $ entryGetText $ snd cObsTimeEntry :: Integer
    putStrLn ("   GPS Time: " ++ (show cGPS) )
    putStrLn ("   Obs Time: " ++ (show cObsTime) )
    {--}
    sig1 <- readFrameFromGPS cGPS cObsTime (activeChannelLabels!!0) cCache
    sig2 <- readFrameFromGPS cGPS cObsTime (activeChannelLabels!!1) cCache
    case (sig1, sig2) of
     (Just x, Just y) -> do
       let corr't = takeCorrelation Pearson x y 10
       putStrLn ("### Not Implemented yet. ###")
       print $ corr't
       plotX Linear Line 1 BLUE ("Time [sec]", "Correlation") 0.05 "CorrelationMon" ((0,0),(0,0)) $ zip [0..] corr't
     (_, _) -> do
       putStrLn ("### Can't read data. ###")
    {----} 
  {-- Exit Process --}
  onDestroy cWindow mainQuit
  widgetShowAll cWindow
  mainGUI
