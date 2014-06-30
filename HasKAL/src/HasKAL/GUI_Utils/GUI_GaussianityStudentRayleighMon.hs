{-******************************************
  *     File Name: GUI_GaussianityStudentRayleighMon.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/06/30 13:30:50
  *******************************************-}

module HasKAL.GUI_Utils.GUI_GaussianityStudentRayleighMon(
   hasKalGuiStudentRayleighMon
) where

import Graphics.UI.Gtk
import qualified Control.Monad as CM -- forM
import qualified Data.Maybe as DM --fromJust
import qualified System.IO.Unsafe as SIOU -- unsafePerformIO

import qualified HasKAL.FrameUtils.FrameUtils as HFF -- 将来的には無くす
import qualified HasKAL.FrameUtils.PickUpFileName as HFP
import qualified HasKAL.GUI_Utils.GUI_Supplement as HGGS
import qualified HasKAL.PlotUtils.PlotUtils as HPP
import qualified HasKAL.TimeUtils.GPSfunction as HTG
import qualified HasKAL.MonitorUtils.SRMon.StudentRayleighMon as SRM
import qualified HasKAL.SpectrumUtils.SpectrumUtils as HSS

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

  srMonCacheOpener <- HGGS.fileOpenButtonNewWithLabelDefault "Cache file" $ HGGS.haskalOpt ++ "/cachefiles/gwffiles_sorted.lst"
  srMonDateCombo <- HGGS.dateComboNew (2014, 3, 17, 16, 15, 12, "JST")
  srMonObsTimeEntry <- HGGS.entryNewWithLabelDefault "OBS Time [s]" "256"
  srMonChunckEntry <- HGGS.entryNewWithLabelDefault "Chunk size" "2097152"
  srMonOverlapEntry <- HGGS.entryNewWithLabelDefault "Overlap(0 < p < 1)" "0.5"
  srMonSamplingEntry <- HGGS.entryNewWithLabelDefault "fsample [Hz]" "16384.0"
  srMonStrideEntry <- HGGS.entryNewWithLabelDefault "Stride Num" "32768"
  srMonFClustEntry <- HGGS.entryNewWithLabelDefault "f Clustering Num" "32"
  srMonClose <- buttonNewWithLabel "Close"
  srMonExecute <- buttonNewWithLabel "Execute"

  {--  Set Parameters of the objects  --}
  set srMonWindow [ windowTitle := "StudentRayleighMon",
                      windowDefaultWidth := 300,
                      windowDefaultHeight := 450,
                      containerChild := srMonVBox,
                      containerBorderWidth := 20 ]

  {--  Arrange object in window  --}
  boxPackStartDefaults srMonVBox srMonHBoxCache
  HGGS.boxPackStartDefaultsPair srMonHBoxCache srMonCacheOpener
  mapM (boxPackStartDefaults srMonVBox) srMonHBoxDate
  CM.zipWithM HGGS.boxPackStartDefaultsPair srMonHBoxDate $ srMonDateCombo
  boxPackStartDefaults srMonVBox srMonHBoxObsTime
  HGGS.boxPackStartDefaultsPair srMonHBoxObsTime srMonObsTimeEntry

  boxPackStartDefaults srMonVBox srMonHBoxChunck
  HGGS.boxPackStartDefaultsPair srMonHBoxChunck srMonChunckEntry
  boxPackStartDefaults srMonVBox srMonHBoxOverlap
  HGGS.boxPackStartDefaultsPair srMonHBoxOverlap srMonOverlapEntry

  boxPackStartDefaults srMonVBox srMonHBoxSampling
  HGGS.boxPackStartDefaultsPair srMonHBoxSampling srMonSamplingEntry
  boxPackStartDefaults srMonVBox srMonHBoxStride
  HGGS.boxPackStartDefaultsPair srMonHBoxStride srMonStrideEntry
  boxPackStartDefaults srMonVBox srMonHBoxFClust
  HGGS.boxPackStartDefaultsPair srMonHBoxFClust srMonFClustEntry
  boxPackStartDefaults srMonVBox srMonHBoxButtons
  mapM (boxPackStartDefaults srMonHBoxButtons) [srMonClose, srMonExecute]

  {--  Execute --}
  onClicked srMonClose $ do
    putStrLn "Closed StudentRayleighMon Window"
    widgetDestroy srMonWindow
  onClicked srMonExecute $ do
    putStrLn "Execute"
    let srmCache = DM.fromJust $ SIOU.unsafePerformIO $ fileChooserGetFilename $ snd srMonCacheOpener
        srmDate = HGGS.dateStr2Tuple $ map (DM.fromJust.SIOU.unsafePerformIO.comboBoxGetActiveText.snd) srMonDateCombo
        srmGPS = read $ HTG.timetuple2gps srmDate :: Integer
        srmObsTime = read $ SIOU.unsafePerformIO $ entryGetText $ snd srMonObsTimeEntry :: Integer

        srmChunck = read $ SIOU.unsafePerformIO $ entryGetText $ snd srMonChunckEntry :: Int
        srmOverlap = read $ SIOU.unsafePerformIO $ entryGetText $ snd srMonOverlapEntry :: Double

        srmSampling = read $ SIOU.unsafePerformIO $ entryGetText $ snd srMonSamplingEntry :: Double
        srmStride = read $ SIOU.unsafePerformIO $ entryGetText $ snd srMonStrideEntry :: Int
        srmFClust = read $ SIOU.unsafePerformIO $ entryGetText $ snd srMonFClustEntry :: Int
        dF = (fromIntegral srmFClust) * srmSampling / (realToFrac srmStride)
    putStrLn ("   GPS Time: " ++ (show srmGPS) )
    putStrLn ("   Obs Time: " ++ (show srmObsTime) )
    putStrLn ("    fsample: " ++ (show srmSampling) )
    putStrLn ("     stride: " ++ (show srmStride) )
{--}
    let snf = getAvePsdFromGPS srmStride srmSampling 32 srmGPS (activeChannelLabels !! 0) srmCache
        frData = getDataFromGPS srmGPS srmObsTime (activeChannelLabels !! 0) srmCache
        nu = SRM.studentRayleighMon srmChunck srmOverlap srmStride srmFClust srmSampling snf frData
    HPP.scatter_plot_2d "Student-RayleighMon" "nu" 2.0 (720,480) $ zip [0, dF..1024] $ (nu !! 0)
{----}

  {--  Exit Process  --}
  onDestroy srMonWindow mainQuit
  widgetShowAll srMonWindow
  mainGUI


{-- Supplementary Functions --}
readFrame' :: String -> String -> IO [Double]
readFrame' = (CM.liftM ((map realToFrac).HFF.eval).).HFF.readFrame

flip231 :: (a -> b -> c -> d) -> b -> c -> a -> d
flip231 = (flip.).flip

getAvePsdFromGPS :: Int -> Double -> Int -> Integer -> String -> String -> [Double]
getAvePsdFromGPS numT fs aveNum gpsD channel cache = map snd.(flip231 HSS.gwpsd numT fs).(take $ aveNum*numT).concat $ datW
  where datW = SIOU.unsafePerformIO $ mapM (readFrame' channel) $ HFP.pickUpFileNameinFile gpsW (gpsD-1) cache
        gpsW = (-) gpsD $ ceiling $ (fromIntegral $ aveNum*numT) / fs

getDataFromGPS :: Integer -> Integer -> String -> String -> [Double]
getDataFromGPS gpsD obsD channel cache = concat $ SIOU.unsafePerformIO $ mapM (readFrame' channel) filelist
  where filelist = HFP.pickUpFileNameinFile gpsD (gpsD+obsD-1) cache
