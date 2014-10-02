{-******************************************
  *     File Name: GUI_GaussianityRayleighMon.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/10/01 19:45:20
  *******************************************-}

module HasKAL.GUI_Utils.GUI_GaussianityRayleighMon(
   hasKalGuiRayleighMon
) where

import Graphics.UI.Gtk
import qualified Control.Monad as CM -- forM
import qualified Data.Maybe as DM -- fromJust
import qualified System.IO.Unsafe as SIOU -- unsafePerformIO

import qualified HasKAL.ExternalUtils.GSL.RandomNumberDistributions as RND
import qualified HasKAL.FrameUtils.FrameUtils as HFF
import qualified HasKAL.FrameUtils.PickUpFileName as HFP
import qualified HasKAL.GUI_Utils.GUI_Supplement as HGGS
import qualified HasKAL.MonitorUtils.RayleighMon.RayleighMon as HMRRM
import qualified HasKAL.PlotUtils.HROOT.PlotGraph as RPG
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
  boxPackStartDefaults rayleighMonVBox rayleighMonHBoxCache
  HGGS.boxPackStartDefaultsPair rayleighMonHBoxCache rayleighMonCacheOpener
  mapM (boxPackStartDefaults rayleighMonVBox) rayleighMonHBoxDate
  CM.zipWithM HGGS.boxPackStartDefaultsPair rayleighMonHBoxDate $ rayleighMonDateCombo
  boxPackStartDefaults rayleighMonVBox rayleighMonHBoxObsTime
  HGGS.boxPackStartDefaultsPair rayleighMonHBoxObsTime rayleighMonObsTimeEntry
  boxPackStartDefaults rayleighMonVBox rayleighMonHBoxSampling
  HGGS.boxPackStartDefaultsPair rayleighMonHBoxSampling rayleighMonSamplingEntry
  boxPackStartDefaults rayleighMonVBox rayleighMonHBoxStride
  HGGS.boxPackStartDefaultsPair rayleighMonHBoxStride rayleighMonStrideEntry
  boxPackStartDefaults rayleighMonVBox rayleighMonHBoxFClust
  HGGS.boxPackStartDefaultsPair rayleighMonHBoxFClust rayleighMonFClustEntry
  boxPackStartDefaults rayleighMonVBox rayleighMonHBoxButtons
  mapM (boxPackStartDefaults rayleighMonHBoxButtons) [rayleighMonClose, rayleighMonExecute]

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
    let snf = getAvePsdFromGPS rmStride rmSampling 32 rmGPS (activeChannelLabels !! 0) rmCache
        frData = getDataFromGPS rmGPS rmObsTime (activeChannelLabels !! 0) rmCache
        quantiles = HMRRM.rayleighMon rmStride rmFClust rmSampling 0.95 snf frData
        theorem = RND.gslCdfRayleighPinv 0.95 1.0
    RPG.oPlotX RPG.Linear RPG.LinePoint ("frequency [Hz]","Normalized noise level [/rHz]") "RayleighMon: p=0.95" $
              [zip [0, dF..rmSampling/4] quantiles, zip [-1, rmSampling] $ repeat theorem]
{----}

  {--  Exit Process  --}
  onDestroy rayleighMonWindow mainQuit
  widgetShowAll rayleighMonWindow
  mainGUI


{-- Supplementary Functions --}
readFrame' :: String -> String -> IO [Double]
readFrame' = (CM.liftM ((map realToFrac).HFF.eval).).HFF.readFrame

getAvePsdFromGPS :: Int -> Double -> Int -> Integer -> String -> String -> [Double]
getAvePsdFromGPS numT fs aveNum gpsD channel cache = map snd.(HMF.flip231 HSS.gwpsd numT fs).(take $ aveNum*numT).concat $ datW
  where datW = SIOU.unsafePerformIO $ mapM (readFrame' channel) $ HFP.pickUpFileNameinFile gpsW (gpsD-1) cache
        gpsW = (-) gpsD $ ceiling $ (fromIntegral $ aveNum*numT) / fs

getDataFromGPS :: Integer -> Integer -> String -> String -> [Double]
getDataFromGPS gpsD obsD channel cache = concat $ SIOU.unsafePerformIO $ mapM (readFrame' channel) filelist
  where filelist = HFP.pickUpFileNameinFile gpsD (gpsD+obsD-1) cache
