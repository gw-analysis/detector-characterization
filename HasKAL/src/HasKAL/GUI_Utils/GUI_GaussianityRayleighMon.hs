{-******************************************
  *     File Name: GUI_GaussianityRayleighMon.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/06/24 18:05:16
  *******************************************-}

module HasKAL.GUI_Utils.GUI_GaussianityRayleighMon(
   hasKalGuiRayleighMon
) where

import Graphics.UI.Gtk
import qualified Control.Monad as CM -- forM
import qualified Data.Maybe as DM -- fromJust
import qualified System.IO.Unsafe as SIOU -- unsafePerformIO

import qualified HasKAL.FrameUtils.FrameUtils as HFF
import qualified HasKAL.FrameUtils.PickUpFileName as HFP
import qualified HasKAL.GUI_Utils.GUI_Supplement as HGGS
import qualified HasKAL.MonitorUtils.RayleighMon.RayleighMon as HMRRM
import qualified HasKAL.PlotUtils.PlotUtils as HPP
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

  rayleighMonCacheOpener <- HGGS.fileOpenButtonNewWithLabelDefault "Cache file" $ HGGS.haskalOpt ++ "/cachefiles/gwffiles_sorted.lst"
  rayleighMonDateCombo <- HGGS.dateComboNew (2014, 3, 17, 16, 15, 12, "JST")
  rayleighMonObsTimeEntry <- HGGS.entryNewWithLabelDefault "OBS Time [s]" "128"
  rayleighMonSamplingEntry <- HGGS.entryNewWithLabelDefault "fsample [Hz]" "16384.0"
  rayleighMonStrideEntry <- HGGS.entryNewWithLabelDefault "Stride Num" "65536"
  rayleighMonFClustEntry <- HGGS.entryNewWithLabelDefault "f Clustering Num" "64"
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
        rmDate = HGGS.dateStr2Tuple $ map (DM.fromJust.SIOU.unsafePerformIO.comboBoxGetActiveText.snd) rayleighMonDateCombo
        rmGPS = read $ HTG.timetuple2gps rmDate :: Integer
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
        quantiles = HMRRM.rayleighMon' rmStride rmFClust rmSampling 0.95 snf frData
    HPP.scatter_plot_2d "RayleighMon 0.95 quantile" "x" 2.0 (720,480) $ zip [0, dF..rmSampling/2] quantiles
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
