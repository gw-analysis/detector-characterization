


module HasKAL.GUI_Utils.GUI_RangeInspiral(
   hasKalGuiInspiralRange
) where

import Graphics.UI.Gtk
import qualified Control.Monad as CM -- forM
import qualified System.IO.Unsafe as SIOU -- unsafePerformIO

import qualified HasKAL.GUI_Utils.GUI_Supplement as HGGS
import qualified HasKAL.MonitorUtils.RangeMon.InspiralRingdownDistanceQuanta as HMRIRD
import qualified HasKAL.PlotUtils.HROOT.PlotGraph as RPG
import qualified HasKAL.TimeUtils.GPSfunction as HTG

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
  inspiralRangeHBoxDate <- mapM (hBoxNew True) $ take 7 [5..]
  inspiralRangeHBoxObsTime <- hBoxNew True 5
  inspiralRangeHBoxMass1 <- hBoxNew True 5
  inspiralRangeHBoxMass2 <- hBoxNew True 5
  inspiralRangeHBoxButtons <- hBoxNew True 5

  inspiralRangeDateCombo <- HGGS.dateComboNew (2014, 3, 17, 16, 15, 12, "JST")
  inspiralRangeObsTimeEntry <- HGGS.entryNewWithLabelDefault "OBS Time [s]" "300"
  inspiralRangeMass1Entry <- HGGS.entryNewWithLabelDefault "mass_min [M_solor]" "1.0"
  inspiralRangeMass2Entry <- HGGS.entryNewWithLabelDefault "mass_max [M_solor]" "70.0"
  inspiralRangeClose <- buttonNewWithLabel "Close"
  inspiralRangeExecute <- buttonNewWithLabel "Execute"

  {--  Set Parameters of the objects  --}
  set inspiralRangeWindow [ windowTitle := "Inspiral Range",
                      windowDefaultWidth := 200,
                      windowDefaultHeight := 450,
                      containerChild := inspiralRangeVBox,
                      containerBorderWidth := 20 ]

  {--  Arrange object in window  --}
  mapM (\x -> boxPackStart inspiralRangeVBox x PackGrow 0) inspiralRangeHBoxDate
  CM.zipWithM HGGS.boxPackStartDefaultsPair inspiralRangeHBoxDate $ inspiralRangeDateCombo
  HGGS.boxPackStartDefaultsPair inspiralRangeHBoxObsTime inspiralRangeObsTimeEntry
  boxPackStart inspiralRangeVBox inspiralRangeHBoxMass1 PackGrow 0
  HGGS.boxPackStartDefaultsPair inspiralRangeHBoxMass1 inspiralRangeMass1Entry
  boxPackStart inspiralRangeVBox inspiralRangeHBoxMass2 PackGrow 0
  HGGS.boxPackStartDefaultsPair inspiralRangeHBoxMass2 inspiralRangeMass2Entry
  boxPackStart inspiralRangeVBox inspiralRangeHBoxButtons PackGrow 0
  mapM (\x -> boxPackStart inspiralRangeHBoxButtons x PackGrow 0) [inspiralRangeClose, inspiralRangeExecute]

  {--  Execute  --}
  onClicked inspiralRangeClose $ do
    putStrLn "Closed InspiralRange Window"
    widgetDestroy inspiralRangeWindow
  onClicked inspiralRangeExecute $ do
    putStrLn "Execute"
    inspDate <- CM.liftM HGGS.dateStr2Tuple $ mapM HGGS.comboBoxGetActiveString inspiralRangeDateCombo
    let inspGPS = HTG.timetuple2gps inspDate
        inspObsTime = read $ SIOU.unsafePerformIO $ entryGetText $ snd inspiralRangeObsTimeEntry :: Int
        inspMass1 = read $ SIOU.unsafePerformIO $ entryGetText $ snd inspiralRangeMass1Entry :: Double
        inspMass2 = read $ SIOU.unsafePerformIO $ entryGetText $ snd inspiralRangeMass2Entry :: Double
    putStrLn ("   GPS Time: " ++ inspGPS)
    putStrLn ("   Obs Time: " ++ (show inspObsTime) )
    putStrLn ("     Mass_1: " ++ (show inspMass1) )
    putStrLn ("     Mass_2: " ++ (show inspMass2) )

    {-- detecter data IO and format --}
    detDataStr <- readFile $ HGGS.haskalOpt ++ "/sensitivities/bKAGRA/prebKAGRA.dat" -- ファイルは[Hz], [/rHz]が書かれているので2乗して使う
    let detData = map HGGS.amp2psd $ map HGGS.convert_LtoT2 $ map (map read) $ map words $ lines detDataStr :: [(Double, Double)]
    {-- end of detecter data IO and format --}
    {-- Monitor tool --}
    inspDist <- CM.forM [inspMass1, inspMass1+2..inspMass2] $ \mass ->
      return $ HMRIRD.distInspiral mass mass detData
    RPG.plotX RPG.LogXY RPG.Line 2 RPG.BLUE ("m1 = m2 [M_sol]", "Distance [Mpc]") 0.05 "Inspiral Range" ((0,0),(0,0)) $ zip [inspMass1,inspMass1+2..inspMass2] inspDist
    {-- End of Monitor Tool --}

  {--  Exit Process  --}
  onDestroy inspiralRangeWindow mainQuit
  widgetShowAll inspiralRangeWindow
  mainGUI

