{-******************************************
  *     File Name: GUI_AntennaPattern.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/08/09 00:11:43
  *******************************************-}

module HasKAL.GUI_Utils.GUI_AntennaPattern (
   hasKalGuiAntennaPattern
) where

import Graphics.UI.Gtk
import qualified Data.Maybe as DM
import qualified System.IO.Unsafe as SIOU
import qualified Control.Monad as CM


import qualified HasKAL.GUI_Utils.GUI_Supplement as HGGS
import qualified HasKAL.DetectorUtils.DetectorParam as DDP
import qualified HasKAL.DetectorUtils.Functions as DUF
import qualified HasKAL.PlotUtils.HROOT.PlotGraph3D as RPG3

{-- Test Code --}
-- main = hasKalGuiAntennaPattern

{-- External Functions --}
hasKalGuiAntennaPattern :: IO ()
hasKalGuiAntennaPattern = do
  initGUI
  putStrLn "Open AntennaPattern Window"
  
  {--  Create new object  --}
  window <- windowNew
  vBox <- vBoxNew True 5
  hBoxDetector <- hBoxNew True 5
  hBoxPsi <- hBoxNew True 5
  hBoxPhi <- hBoxNew True 5
  hBoxTheta <- hBoxNew True 5
  hBoxButtons <- hBoxNew True 5

  detectorCombo <- HGGS.comboBoxNewLabelAppendTexts "Detector" ["KAGRA", "LIGO Hanford", "LIGO Livingston", "Virgo", "GEO600"] 0
  psiEntry <- HGGS.entryNewWithLabelDefault "polarization angle [deg.]" "0.0"
  phiEntry <- HGGS.entryNewWithLabelDefault "delta phi [deg.]" "1.0"
  thetaEntry <- HGGS.entryNewWithLabelDefault "delta theta [deg.]" "1.0"
  closeButton <- buttonNewWithLabel "Close"
  executeButton <- buttonNewWithLabel "Execute"

  {--  set parameter of the objects --}
  set window [windowTitle := "Antenna Pattern",
              windowDefaultWidth := 200,
              windowDefaultHeight := 300,
              containerChild := vBox,
              containerBorderWidth := 20 ]

  {--  Arrange object in window  --}
  boxPackStartDefaults vBox hBoxDetector
  HGGS.boxPackStartDefaultsPair hBoxDetector detectorCombo
  boxPackStartDefaults vBox hBoxPsi
  HGGS.boxPackStartDefaultsPair hBoxPsi psiEntry
  boxPackStartDefaults vBox hBoxPhi
  HGGS.boxPackStartDefaultsPair hBoxPhi phiEntry
  boxPackStartDefaults vBox hBoxTheta
  HGGS.boxPackStartDefaultsPair hBoxTheta thetaEntry
  boxPackStartDefaults vBox hBoxButtons
  mapM (boxPackStartDefaults hBoxButtons) [closeButton, executeButton]

  {--  Execute  --}
  onClicked closeButton $ do
    putStrLn "Closed AntennaPattern Window"
    widgetDestroy window
  onClicked executeButton $ do
    putStrLn "Execute"
    let detectorStr = (DM.fromJust.SIOU.unsafePerformIO.comboBoxGetActiveText.snd) detectorCombo
        psiD = abs.read.SIOU.unsafePerformIO.entryGetText.snd $ psiEntry :: Double
        dPhiD = abs.read.SIOU.unsafePerformIO.entryGetText.snd $ phiEntry :: Double
        dThetaD = abs.read.SIOU.unsafePerformIO.entryGetText.snd $ thetaEntry :: Double
    putStrLn $ "   Detector: " ++ show detectorStr
    putStrLn $ "        Psi: " ++ show psiD
    putStrLn $ "  delta Phi: " ++ show dPhiD
    putStrLn $ "delta Theta: " ++ show dThetaD
    let phiV = [-180.0, (-180.0+dPhiD)..180.0]
        thetaV = [-90.0, (-90.0+dThetaD)..90.0]
    skymap <- CM.forM phiV $ \phi ->
      CM.forM thetaV $ \theta -> do
        let fpfc = DUF.fplusfcrossts (detStr2Param detectorStr) phi theta psiD
        return $ sqrt $ (fst3 fpfc)**2 + (snd3 fpfc)**2
    RPG3.skyMapX RPG3.Linear RPG3.COLZ "Z" ("Antenna Pattern Skymap at "++detectorStr) $ genSkymapData phiV thetaV skymap

  {--  Exit Process  --}
  onDestroy window mainQuit
  widgetShowAll window
  mainGUI

{--  Internal Functions  --}
detStr2Param :: String -> DDP.DetectorParam
detStr2Param detector 
  | detector == "KAGRA" = DDP.kagra
  | detector == "LIGO Hanford" = DDP.ligoHanford
  | detector == "LIGO Livingston" = DDP.ligoLivingston
  | detector == "Virgo" = DDP.virgo
  | detector == "GEO600" = DDP.geo600

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x
snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

genSkymapData :: [Double] -> [Double] -> [[Double]] -> [(Double,  Double,  Double)]
genSkymapData phiV thetaV skymap = do
  let phiV' = concat [ replicate (length thetaV) x | x <- phiV]
      thetaV'=take (length phiV * length thetaV) $ cycle thetaV
  zip3 phiV' thetaV' (concat skymap)
