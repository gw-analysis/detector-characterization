{- |
Module      : HasKAL.GUI_Utils.GUI_GaussianityStudentRayleighMon
Description : This is documentation tests.
Copyright   : (c) WhoAmI, 2014
License     : ???
Maintainer  : hoge@hoge.com
Stability   : test
Portability : POSIX

GUI of Student-Rayleigh Monitor

-}

module HasKAL.GUI_Utils.GUI_GaussianityStudentRayleighMon(
   hasKalGuiStudentRayleighMon
) where

import Graphics.UI.Gtk
import qualified Control.Monad as CM -- forM
import qualified Data.Maybe as DM --fromJust
import qualified System.IO.Unsafe as SIOU -- unsafePerformIO

import qualified HasKAL.FrameUtils.Function as HFF
import qualified HasKAL.GUI_Utils.GUI_Supplement as HGGS
import qualified HasKAL.PlotUtils.HROOT.PlotGraph3D as PG3
import qualified HasKAL.TimeUtils.GPSfunction as HTG
import qualified HasKAL.MonitorUtils.SRMon.StudentRayleighMon as SRM
import qualified HasKAL.SpectrumUtils.SpectrumUtils as HSS
import qualified HasKAL.Misc.Flip3param as HMF

{-- StudentRayleighMon Window 
-- test code
main = IO ()
main = hasKalGuiStudnetRayleighMon ["Channel_Name"]
-- arguments: channel_name
--}
hasKalGuiStudentRayleighMon :: [String] -> IO ()
hasKalGuiStudentRayleighMon activeChannelLabels = do
  initGUI  
  putStrLn "Open StudentRayLeigMon Window"

  {--  Create new object --}
  srMonWindow <- windowNew
  srMonVBox <- vBoxNew True 5
  srMonHBoxCache <- hBoxNew True 5
  srMonHBoxDate <- mapM (hBoxNew True) $ take 7 [5..]
  srMonHBoxObsTime <- hBoxNew True 5
  srMonHBoxChunck <- hBoxNew True 5
  srMonHBoxOverlap <- hBoxNew True 5
  srMonHBoxSampling <- hBoxNew True 5
  srMonHBoxStride <- hBoxNew True 5
  srMonHBoxFClust <- hBoxNew True 5
  srMonHBoxButtons <- hBoxNew True 5

  srMonCacheOpener <- HGGS.fileOpenButtonNewWithLabelDefault "Cache file" $ HGGS.haskalOpt ++ "/cachefiles/cachefile_20140702.lst"
  srMonDateCombo <- HGGS.dateComboNew (2014, 5, 17, 16, 15, 12, "JST")
  srMonObsTimeEntry <- HGGS.entryNewWithLabelDefault "OBS Time [s]" "4096"
  srMonChunckEntry <- HGGS.entryNewWithLabelDefault "Chunk size" "128"
  srMonOverlapEntry <- HGGS.entryNewWithLabelDefault "Overlap(0 < p < 1)" "0.875"
  srMonSamplingEntry <- HGGS.entryNewWithLabelDefault "fsample [Hz]" "16384.0"
  srMonStrideEntry <- HGGS.entryNewWithLabelDefault "Stride Num" "16384"
  srMonFClustEntry <- HGGS.entryNewWithLabelDefault "f Clustering Num" "16"
  srMonClose <- buttonNewWithLabel "Close"
  srMonExecute <- buttonNewWithLabel "Execute"

  {--  Set Parameters of the objects  --}
  set srMonWindow [ windowTitle := "StudentRayleighMon",
                      windowDefaultWidth := 300,
                      windowDefaultHeight := 450,
                      containerChild := srMonVBox,
                      containerBorderWidth := 20 ]

  {--  Arrange object in window  --}
  boxPackStart srMonVBox srMonHBoxCache PackGrow 0
  HGGS.boxPackStartDefaultsPair srMonHBoxCache srMonCacheOpener
  mapM (\x -> boxPackStart srMonVBox x PackGrow 0) srMonHBoxDate
  CM.zipWithM HGGS.boxPackStartDefaultsPair srMonHBoxDate $ srMonDateCombo
  boxPackStart srMonVBox srMonHBoxObsTime PackGrow 0
  HGGS.boxPackStartDefaultsPair srMonHBoxObsTime srMonObsTimeEntry
  boxPackStart srMonVBox srMonHBoxChunck PackGrow 0
  HGGS.boxPackStartDefaultsPair srMonHBoxChunck srMonChunckEntry
  boxPackStart srMonVBox srMonHBoxOverlap PackGrow 0
  HGGS.boxPackStartDefaultsPair srMonHBoxOverlap srMonOverlapEntry
  boxPackStart srMonVBox srMonHBoxSampling PackGrow 0
  HGGS.boxPackStartDefaultsPair srMonHBoxSampling srMonSamplingEntry
  boxPackStart srMonVBox srMonHBoxStride PackGrow 0
  HGGS.boxPackStartDefaultsPair srMonHBoxStride srMonStrideEntry
  boxPackStart srMonVBox srMonHBoxFClust PackGrow 0
  HGGS.boxPackStartDefaultsPair srMonHBoxFClust srMonFClustEntry
  boxPackStart srMonVBox srMonHBoxButtons PackGrow 0
  mapM (\x -> boxPackStart srMonHBoxButtons x PackGrow 0) [srMonClose, srMonExecute]

  {--  Execute --}
  onClicked srMonClose $ do
    putStrLn "Closed StudentRayleighMon Window"
    widgetDestroy srMonWindow
  onClicked srMonExecute $ do
    putStrLn "Execute"
    let srmCache = DM.fromJust $ SIOU.unsafePerformIO $ fileChooserGetFilename $ snd srMonCacheOpener
    srmDate <- CM.liftM HGGS.dateStr2Tuple $ mapM HGGS.comboBoxGetActiveString srMonDateCombo
    let srmGPS = read $ HTG.timetuple2gps srmDate :: Integer
        srmObsTime = read $ SIOU.unsafePerformIO $ entryGetText $ snd srMonObsTimeEntry :: Integer
        srmChunck = read $ SIOU.unsafePerformIO $ entryGetText $ snd srMonChunckEntry :: Int
        srmOverlap = read $ SIOU.unsafePerformIO $ entryGetText $ snd srMonOverlapEntry :: Double
        srmSampling = read $ SIOU.unsafePerformIO $ entryGetText $ snd srMonSamplingEntry :: Double
        srmStride = read $ SIOU.unsafePerformIO $ entryGetText $ snd srMonStrideEntry :: Int
        srmFClust = read $ SIOU.unsafePerformIO $ entryGetText $ snd srMonFClustEntry :: Int
        shiftN = truncate $ (1.0 - srmOverlap) * fromIntegral srmChunck
        -- dF = (fromIntegral srmFClust) * srmSampling / (realToFrac srmStride)
        -- dT = fromIntegral srmChunck / srmSampling * (1.0 - srmOverlap)
    putStrLn ("   GPS Time: " ++ (show srmGPS) )
    putStrLn ("   Obs Time: " ++ (show srmObsTime) )
    putStrLn ("    fsample: " ++ (show srmSampling) )
    putStrLn ("     stride: " ++ (show srmStride) )
{--}
    snt <- HFF.readFrameFromGPS'V srmGPS srmObsTime (activeChannelLabels!!0) srmCache
    let snf = HSS.gwpsdV snt srmStride srmSampling
    hts <- HFF.readFrameFromGPS'V srmGPS srmObsTime (activeChannelLabels!!0) srmCache
    let hfs = HSS.gwspectrogramV 0 srmStride srmSampling hts
        nus = SRM.studentRayleighMonV (SRM.QUANT 0.99) srmSampling srmStride srmChunck shiftN srmFClust snf hfs
    PG3.spectrogramMX PG3.LogY PG3.COLZ "nu" "SRMon" nus
{----}

  {--  Exit Process  --}
  onDestroy srMonWindow mainQuit
  widgetShowAll srMonWindow
  mainGUI

