{-# HADDOCK Markdown #-}
{- |
Module      : HasKAL.GUI_Utils.GUI_RangeIMBH
Description : This is documentation tests.
Copyright   : (c) WhoAmI, 2014
License     : ???
Maintainer  : hoge@hoge.com
Stability   : test
Portability : POSIX

GUI of IMBH Range Monitor

-}

module HasKAL.GUI_Utils.GUI_RangeIMBH(
   hasKalGuiIMR'Range
) where

import Graphics.UI.Gtk
import qualified Control.Monad as CM -- forM
import qualified System.IO.Unsafe as SIOU -- unsafePerformIO

import qualified HasKAL.GUI_Utils.GUI_Supplement as HGGS
import qualified HasKAL.MonitorUtils.RangeMon.IMBH as HMRIMBHD
import qualified HasKAL.PlotUtils.HROOT.PlotGraph as RPG
import qualified HasKAL.TimeUtils.GPSfunction as HTG

{--  Inspiral-Marger-RingdownRange Window
-- test code
main = IO ()
main = hasKalGuiIMR'Range
-- arguments: Nothing
--}
hasKalGuiIMR'Range :: IO ()
hasKalGuiIMR'Range = do
  initGUI
  putStrLn "Open Inspiral-Merger-RingdownRange Window"

  {--  Create new object  --}
  imrRangeWindow <- windowNew
  imrRangeVBox <- vBoxNew True 5
  imrRangeHBoxDate <- mapM (hBoxNew True) $ take 7 [5..]
  imrRangeHBoxObsTime <- hBoxNew True 5
  imrRangeHBoxMass1 <- hBoxNew True 5
  imrRangeHBoxMass2 <- hBoxNew True 5
  imrRangeHBoxButtons <- hBoxNew True 5

  imrRangeDateCombo <- HGGS.dateComboNew (2014, 3, 17, 16, 15, 12, "JST")
  imrRangeObsTimeEntry <- HGGS.entryNewWithLabelDefault "OBS Time [s]" "300"
  imrRangeMass1Entry <- HGGS.entryNewWithLabelDefault "mass_min [M_solar]" "1.0"
  imrRangeMass2Entry <- HGGS.entryNewWithLabelDefault "mass_max [M_solar]" "300.0"
  imrRangeClose <- buttonNewWithLabel "Close"
  imrRangeExecute <- buttonNewWithLabel "Execute"

  {--  Set Parameters of the objects  --}
  set imrRangeWindow [ windowTitle := "Inspiral-Merger-Ringdown Range",
                      windowDefaultWidth := 200,
                      windowDefaultHeight := 450,
                      containerChild := imrRangeVBox,
                      containerBorderWidth := 20 ]

  {--  Arrange object in window  --}
  mapM (boxPackStartDefaults imrRangeVBox) imrRangeHBoxDate
  CM.zipWithM HGGS.boxPackStartDefaultsPair imrRangeHBoxDate $ imrRangeDateCombo
  boxPackStartDefaults imrRangeVBox imrRangeHBoxObsTime
  HGGS.boxPackStartDefaultsPair imrRangeHBoxObsTime imrRangeObsTimeEntry
  boxPackStartDefaults imrRangeVBox imrRangeHBoxMass1
  HGGS.boxPackStartDefaultsPair imrRangeHBoxMass1 imrRangeMass1Entry
  boxPackStartDefaults imrRangeVBox imrRangeHBoxMass2
  HGGS.boxPackStartDefaultsPair imrRangeHBoxMass2 imrRangeMass2Entry
  boxPackStartDefaults imrRangeVBox imrRangeHBoxButtons
  mapM (boxPackStartDefaults imrRangeHBoxButtons) [imrRangeClose, imrRangeExecute]

  {--  Execute  --}
  onClicked imrRangeClose $ do
    putStrLn "Closed Inspiral-Merger-RingdownRange Window"
    widgetDestroy imrRangeWindow
  onClicked imrRangeExecute $ do
    putStrLn "Execute"
    imrDate <- CM.liftM HGGS.dateStr2Tuple $ mapM HGGS.comboBoxGetActiveString imrRangeDateCombo
    let imrGPS = HTG.timetuple2gps imrDate
        imrObsTime = read $ SIOU.unsafePerformIO $ entryGetText $ snd imrRangeObsTimeEntry :: Int
        imrMass1 = read $ SIOU.unsafePerformIO $ entryGetText $ snd imrRangeMass1Entry :: Double
        imrMass2 = read $ SIOU.unsafePerformIO $ entryGetText $ snd imrRangeMass2Entry :: Double
    putStrLn ("   GPS Time: " ++ imrGPS)
    putStrLn ("   Obs Time: " ++ (show imrObsTime) )
    putStrLn ("     Mass_1: " ++ (show imrMass1) )
    putStrLn ("     Mass_2: " ++ (show imrMass2) )

    {-- detecter data IO and format --}
    detDataStr <- readFile $ HGGS.haskalOpt ++ "/sensitivities/bKAGRA/prebKAGRA.dat"
    let detData = map HGGS.amp2psd $ map HGGS.convert_LtoT2 $ map (map read) $ map words $ lines detDataStr :: [(Double, Double)]
    {-- end of detecter data IO and format --}
    {-- Monitor tool --}
    imrDist <- CM.forM [imrMass1, imrMass1+10..imrMass2] $ \mass ->
      return $ HMRIMBHD.distImbh mass mass detData
    RPG.plotX  RPG.LogXY RPG.Line 2 RPG.BLUE ("m1 = m2 [M_sol]", "Distance [Mpc]") 0.05 "IMBH Range" ((0,0),(0,0)) $ zip [imrMass1,imrMass1+10..imrMass2] imrDist
    {-- End of Monitor Tool --}

  {--  Exit Process  --}
  onDestroy imrRangeWindow mainQuit
  widgetShowAll imrRangeWindow
  mainGUI


