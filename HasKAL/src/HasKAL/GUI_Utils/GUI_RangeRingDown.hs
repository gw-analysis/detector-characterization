{-# HADDOCK Markdown #-}
{- |
Module      : HasKAL.GUI_Utils.GUI_RangeRingDown
Description : This is documentation tests.
Copyright   : (c) WhoAmI, 2014
License     : ???
Maintainer  : hoge@hoge.com
Stability   : test
Portability : POSIX

GUI of Ring Down Range Monitor

-}

module HasKAL.GUI_Utils.GUI_RangeRingDown(
   hasKalGuiRingDownRange
) where

import Graphics.UI.Gtk
import qualified Control.Monad as CM -- forM
import qualified System.IO.Unsafe as SIOU -- unsafePerformIO

import qualified HasKAL.GUI_Utils.GUI_Supplement as HGGS
import qualified HasKAL.MonitorUtils.RangeMon.InspiralRingdownDistanceQuanta as HMRIRD
import qualified HasKAL.PlotUtils.HROOT.PlotGraph as RPG
import qualified HasKAL.TimeUtils.GPSfunction as HTG

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
  ringDownRangeHBoxDate <- mapM (hBoxNew True) $ take 7 [5..]
  ringDownRangeHBoxObsTime <- hBoxNew True 5
  ringDownRangeHBoxMass1 <- hBoxNew True 5
  ringDownRangeHBoxMass2 <- hBoxNew True 5
  ringDownRangeHBoxButtons <- hBoxNew True 5

  ringDownRangeDateCombo <- HGGS.dateComboNew (2014, 3, 17, 16, 15, 12, "JST")
  ringDownRangeObsTimeEntry <- HGGS.entryNewWithLabelDefault "OBS Time [s]" "300"
  ringDownRangeMass1Entry <- HGGS.entryNewWithLabelDefault "massMin [M_solar]" "10.0"
  ringDownRangeMass2Entry <- HGGS.entryNewWithLabelDefault "massMax [M_solar]" "10000.0"
  ringDownRangeClose <- buttonNewWithLabel "Close"
  ringDownRangeExecute <- buttonNewWithLabel "Execute"

  {--  Set Parameters of the objects  --}
  set ringDownRangeWindow [ windowTitle := "RingDown Range",
                      windowDefaultWidth := 200,
                      windowDefaultHeight := 450,
                      containerChild := ringDownRangeVBox,
                      containerBorderWidth := 20 ]

  {--  Arrange object in window  --}
  mapM (boxPackStartDefaults ringDownRangeVBox) ringDownRangeHBoxDate
  CM.zipWithM HGGS.boxPackStartDefaultsPair ringDownRangeHBoxDate $ ringDownRangeDateCombo
  boxPackStartDefaults ringDownRangeVBox ringDownRangeHBoxObsTime
  HGGS.boxPackStartDefaultsPair ringDownRangeHBoxObsTime ringDownRangeObsTimeEntry
  boxPackStartDefaults ringDownRangeVBox ringDownRangeHBoxMass1
  HGGS.boxPackStartDefaultsPair ringDownRangeHBoxMass1 ringDownRangeMass1Entry
  boxPackStartDefaults ringDownRangeVBox ringDownRangeHBoxMass2
  HGGS.boxPackStartDefaultsPair ringDownRangeHBoxMass2 ringDownRangeMass2Entry
  boxPackStartDefaults ringDownRangeVBox ringDownRangeHBoxButtons
  mapM (boxPackStartDefaults ringDownRangeHBoxButtons) [ringDownRangeClose, ringDownRangeExecute]

  {--  Execute  --}
  onClicked ringDownRangeClose $ do
    putStrLn "Closed RingDownRange Window"
    widgetDestroy ringDownRangeWindow
  onClicked ringDownRangeExecute $ do
    putStrLn "Execute"
    ringDDate <- CM.liftM HGGS.dateStr2Tuple $ mapM HGGS.comboBoxGetActiveString ringDownRangeDateCombo
    let ringDGPS = HTG.timetuple2gps ringDDate
        ringDObsTime = read $ SIOU.unsafePerformIO $ entryGetText $ snd ringDownRangeObsTimeEntry :: Int
        ringDMass1 = read $ SIOU.unsafePerformIO $ entryGetText $ snd ringDownRangeMass1Entry :: Double
        ringDMass2 = read $ SIOU.unsafePerformIO $ entryGetText $ snd ringDownRangeMass2Entry :: Double
    putStrLn ("   GPS Time: " ++ ringDGPS)
    putStrLn ("   Obs Time: " ++ (show ringDObsTime) )
    putStrLn ("   Mass Min: " ++ (show ringDMass1) )
    putStrLn ("   Mass Max: " ++ (show ringDMass2) )

    {-- detecter data IO and format --}
    detDataStr <- readFile $ HGGS.haskalOpt ++ "/sensitivities/bKAGRA/prebKAGRA.dat"
    let detData = map HGGS.amp2psd $ map HGGS.convert_LtoT2 $ map (map read) $ map words $ lines detDataStr :: [(Double, Double)]
    {-- end of detecter data IO and format --}
    {-- Monitor tool --}
    ringDDist <- CM.forM [ringDMass1, 10.0*ringDMass1..ringDMass2] $ \mass -> 
      return $ HMRIRD.distRingdown mass detData
    RPG.plotX  RPG.LogXY RPG.Line 2 RPG.BLUE ("mass [M_sol]", "Distance [Mpc]") 0.05 "Ringdown Range" ((0,0),(0,0)) $ zip [ringDMass1, 10.0*ringDMass1..ringDMass2] ringDDist
    {-- End of Monitor Tool --}

  {--  Exit Process  --}
  onDestroy ringDownRangeWindow mainQuit
  widgetShowAll ringDownRangeWindow
  mainGUI

