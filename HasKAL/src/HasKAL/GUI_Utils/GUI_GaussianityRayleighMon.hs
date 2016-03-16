


module HasKAL.GUI_Utils.GUI_GaussianityRayleighMon(
   hasKalGuiRayleighMon
) where

import Graphics.UI.Gtk
import qualified Control.Monad as CM -- forM
import qualified Data.Maybe as DM -- fromJust
import qualified System.IO.Unsafe as SIOU -- unsafePerformIO

import qualified HasKAL.ExternalUtils.GSL.RandomNumberDistributions as RND
import qualified HasKAL.FrameUtils.Function as HFF
import qualified HasKAL.GUI_Utils.GUI_Supplement as HGGS
import qualified HasKAL.MonitorUtils.RayleighMon.RayleighMon as RM
import HasKAL.PlotUtils.HROOT.PlotGraph
import qualified HasKAL.TimeUtils.GPSfunction as HTG
import qualified HasKAL.Misc.Flip3param as HMF

import qualified HasKAL.SpectrumUtils.SpectrumUtils as HSS

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
  rayleighMonHBoxCache <- hBoxNew True 5
  rayleighMonHBoxDate <- mapM (hBoxNew True) $ take 7 [5..]
  rayleighMonHBoxObsTime <- hBoxNew True 5
  rayleighMonHBoxSampling <- hBoxNew True 5
  rayleighMonHBoxStride <- hBoxNew True 5
  rayleighMonHBoxFClust <- hBoxNew True 5
  rayleighMonHBoxButtons <- hBoxNew True 5

  rayleighMonCacheOpener <- HGGS.fileOpenButtonNewWithLabelDefault "Cache file" $ HGGS.haskalOpt ++ "/cachefiles/cliocache.lst"
  rayleighMonDateCombo <- HGGS.dateComboNew (2012, 10, 18, 9, 25, 52, "JST")--Oct 18 00:25:52 2012
  rayleighMonObsTimeEntry <- HGGS.entryNewWithLabelDefault "OBS Time [s]" "128"
  rayleighMonSamplingEntry <- HGGS.entryNewWithLabelDefault "fsample [Hz]" "16384.0"
  rayleighMonStrideEntry <- HGGS.entryNewWithLabelDefault "Stride Num" "65536"
  rayleighMonFClustEntry <- HGGS.entryNewWithLabelDefault "f Clustering Num" "16"
  rayleighMonClose <- buttonNewWithLabel "Close"
  rayleighMonExecute <- buttonNewWithLabel "Execute"

  {--  Set Parameters of the objects  --}
  set rayleighMonWindow [ windowTitle := "RayleighMon",
                      windowDefaultWidth := 300,
                      windowDefaultHeight := 450,
                      containerChild := rayleighMonVBox,
                      containerBorderWidth := 20 ]

  {--  Arrange object in window  --}
  boxPackStart rayleighMonVBox rayleighMonHBoxCache PackGrow 0
  HGGS.boxPackStartDefaultsPair rayleighMonHBoxCache rayleighMonCacheOpener
  mapM (\x -> boxPackStart rayleighMonVBox x PackGrow 0) rayleighMonHBoxDate
  CM.zipWithM HGGS.boxPackStartDefaultsPair rayleighMonHBoxDate $ rayleighMonDateCombo
  boxPackStart rayleighMonVBox rayleighMonHBoxObsTime PackGrow 0
  HGGS.boxPackStartDefaultsPair rayleighMonHBoxObsTime rayleighMonObsTimeEntry
  boxPackStart rayleighMonVBox rayleighMonHBoxSampling PackGrow 0
  HGGS.boxPackStartDefaultsPair rayleighMonHBoxSampling rayleighMonSamplingEntry
  boxPackStart rayleighMonVBox rayleighMonHBoxStride PackGrow 0
  HGGS.boxPackStartDefaultsPair rayleighMonHBoxStride rayleighMonStrideEntry
  boxPackStart rayleighMonVBox rayleighMonHBoxFClust PackGrow 0
  HGGS.boxPackStartDefaultsPair rayleighMonHBoxFClust rayleighMonFClustEntry
  boxPackStart rayleighMonVBox rayleighMonHBoxButtons PackGrow 0
  mapM (\x -> boxPackStart rayleighMonHBoxButtons x PackGrow 0) [rayleighMonClose, rayleighMonExecute]

  {--  Execute --}
  onClicked rayleighMonClose $ do
    putStrLn "Closed RayleighMon Window"
    widgetDestroy rayleighMonWindow
  onClicked rayleighMonExecute $ do
    putStrLn "Execute"
    let rmCache = DM.fromJust $ SIOU.unsafePerformIO $ fileChooserGetFilename $ snd rayleighMonCacheOpener
    rmDate <- CM.liftM HGGS.dateStr2Tuple $ mapM HGGS.comboBoxGetActiveString rayleighMonDateCombo
    let rmGPS = read $ HTG.timetuple2gps rmDate :: Integer
        rmObsTime = read $ SIOU.unsafePerformIO $ entryGetText $ snd rayleighMonObsTimeEntry :: Integer
        rmSampling = read $ SIOU.unsafePerformIO $ entryGetText $ snd rayleighMonSamplingEntry :: Double
        rmStride = read $ SIOU.unsafePerformIO $ entryGetText $ snd rayleighMonStrideEntry :: Int
        rmFClust = read $ SIOU.unsafePerformIO $ entryGetText $ snd rayleighMonFClustEntry :: Int
        dF = (fromIntegral rmFClust) * rmSampling / (realToFrac rmStride)
    putStrLn ("   GPS Time: " ++ (show rmGPS) )
    putStrLn ("   Obs Time: " ++ (show rmObsTime) )
    putStrLn ("    fsample: " ++ (show rmSampling) )
    putStrLn ("     stride: " ++ (show rmStride) )
{--}
    snt <- HFF.readFrameFromGPS'V rmGPS rmObsTime (activeChannelLabels!!0) rmCache
    hts <- HFF.readFrameFromGPS'V rmGPS rmObsTime (activeChannelLabels!!0) rmCache
    case (snt, hts) of
     (Just x, Just y) -> do
       let snf = HSS.gwpsdV x rmStride rmSampling
           hfs = HSS.gwspectrogramV 0 rmStride rmSampling y
           quant = RM.rayleighMonV [0.5,0.9,0.95,0.99] rmSampling rmStride rmFClust snf hfs
           colors = [RED,RED,GREEN,GREEN,BLUE,BLUE,PINK,PINK]
           lineopts = replicate 8 LinePoint
       oPlotXV LogXY lineopts 2 colors ("frequency [Hz]", "normalized noise level [/rHz]") 0.05 "RMon" ((0,0),(1,5)) $ concatPair quant
     (_, _) -> do
       putStrLn "###  Can't read data  ###"
{----}

  {--  Exit Process  --}
  onDestroy rayleighMonWindow mainQuit
  widgetShowAll rayleighMonWindow
  mainGUI


{-- Supplementary Functions --}
concatPair [] = []
concatPair (x:xs) = [fst x, snd x] ++ concatPair xs

