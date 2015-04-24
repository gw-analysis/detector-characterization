module EventDisplayXend
( channelPlot
, genWebPage
) where


import HasKAL.PlotUtils.HROOT.PlotGraph
import HasKAL.PlotUtils.HROOT.PlotGraph3D
import HasKAL.FrameUtils.Function (readFrameV)
import HasKAL.FrameUtils.FrameUtils (getChannelList, getGPSTime)
import HasKAL.FrameUtils.FileManipulation (extractDataLengthfromFilename)
import HasKAL.SpectrumUtils.SpectrumUtils (gwpsdV, gwspectrogramV, Spectrogram)
import Control.Monad (forM_, liftM)
import Data.List (isSuffixOf, isInfixOf)
import Data.Packed.Vector (fromList, subVector, dim)
import System.Environment (getArgs)
import System.Directory (createDirectoryIfMissing, copyFile, removeFile)
import qualified Numeric.LinearAlgebra as NL
import System.PosixCompat.Files (fileExist, removeLink, createSymbolicLink)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import HasKAL.TimeUtils.GPSfunction (gps2time)
import Data.List (intercalate)
import Data.List.Split (splitOn)

data Paths = Paths
  { sethome :: String
  , setsave :: String
  , setsaveLatest :: String
  } deriving (Show, Eq, Read)

data ChanOutPut = ChanOutPut
  { home :: String
  , save :: String
  , saveLatest :: String
  , gpstime :: String
  , dt :: String
  } deriving (Show, Eq, Read)

{- generate plots of channels in a given frame file -}
channelPlot :: Paths              -- | Path setting
            -> [(String, Double)] -- | channel list to plot [(channel, sampling frequency)]
            -> String             -- | frame file
            -> IO ChanOutPut
channelPlot path chList filename = do
  let homePath = sethome path
      savePath = setsave path
      savePathLatest = setsaveLatest path
  createDirectoryIfMissing True savePath
  createDirectoryIfMissing True savePathLatest
  gTimeS <- liftM (show.fst) $ getGPSTime filename
  let durationS = show $ extractDataLengthfromFilename filename
  chList' <- getChannelList filename
  forM_ chList $ \(channel, fs) -> do
    let plotfname = savePath </> channel++"_TS-"++gTimeS++"-"++durationS++".png"
        plotpsdfname = savePath </> channel++"_PSD-"++gTimeS++"-"++durationS++".png"
        plotspefname = savePath </> channel++"_SPE-"++gTimeS++"-"++durationS++".png"
    xs <- readFrameV channel filename
    case (isSuffixOf "-RAW" channel) of
     True -> do
       plotV Linear Line 1 BLUE ("[s]", "ADC Count") 0.05 channel plotfname
         ((0,0),(0,0)) $ (fromList [1/fs,2/fs..32], xs)
     False -> do
       plotV Linear Line 1 BLUE ("[s]", "[V]") 0.05 channel plotfname
         ((0,0),(0,0)) $ (fromList [1/fs,2/fs..32], xs)
       let (ys, zs) = gwpsdV xs (truncate fs) fs
       plotV LogXY Line 1 BLUE ("[Hz]", "[V/rHz]") 0.04 channel plotpsdfname
         ((0,0),(0,0)) (subVector 0 (dim ys `div` 2 - 1) ys, subVector 0 (dim zs `div` 2 - 1) (sqrt zs))
       spectrogramM LogYZ COLZ " " (channel) plotspefname
         $ setRange 3 1024 $ gwspectrogramV 0 (truncate fs) fs xs
  return $ ChanOutPut { home = homePath
                      , save = savePath
                      , saveLatest = savePathLatest
                      , gpstime = gTimeS
                      , dt = durationS
                      }

genWebPage :: ChanOutPut
           -> [(String, Double)]
           -> IO ()
genWebPage param chList = do
  let homePath = home param
      savePath = save param
      savePathLatest = saveLatest param
      gTimeS = gpstime param
      durationS = dt param
  contents <- readFile $ homePath </> "template.html"
  updatetarget <- genTargetLatest savePath savePathLatest gTimeS durationS chList

  let updatedContents = updateWebPage updatetarget $ updateWebPage (updateTime "GPS_TIME" (read gTimeS :: Integer)) contents
      newhtml = homePath </> "index.html"
  writeFile newhtml updatedContents

replace old new = intercalate new . splitOn old

setRange :: Double -> Double -> Spectrogram -> Spectrogram
setRange flow fhigh spec = do
  let (tv, fv, p) = spec
      flowIndex = head $ NL.find (>=flow) fv
      nrow = NL.rows p
      (tv', fv', p') = (tv, subVector flowIndex (nrow-flowIndex) fv, NL.dropRows flowIndex p)
      fhighIndex = last $ NL.find (<=fhigh) fv'
  (tv', subVector 0 fhighIndex fv', NL.takeRows fhighIndex p')

updateTime :: String -> Integer -> [(String, String)]
updateTime timePlaceHolder gpstime = [(timePlaceHolder, gps2time gpstime)]

genTargetLatest :: String -> String -> String -> String -> [(String, Double)] -> IO [(String, String)]
genTargetLatest _ _ _ _ [] = return []
genTargetLatest savePath savePathLatest gTimeS durationS (ch:chList) = do
  let (channel, _) = ch
      plotfname = savePath </> channel++"_TS-"++gTimeS++"-"++durationS++".png"
      plotpsdfname = savePath </> channel++"_PSD-"++gTimeS++"-"++durationS++".png"
      plotspefname = savePath </> channel++"_SPE-"++gTimeS++"-"++durationS++".png"
  let targetTS | (isInfixOf "ACC_NO2_Y" channel) == True = "SeisEW_TS"
               | (isInfixOf "ACC_NO2_X" channel) == True = "SeisNS_TS"
               | (isInfixOf "ACC_NO2_Z" channel) == True = "SeisZ_TS"
               | (isInfixOf "MAG_Y" channel) == True = "MagEW_TS"
               | (isInfixOf "MAG_X" channel) == True = "MagNS_TS"
               | (isInfixOf "MAG_Z" channel) == True = "MagZ_TS"
               | (isInfixOf "MIC" channel)   == True = "MIC_TS"
               | (isInfixOf "REF" channel)   == True = "DAQ_TS"
               | otherwise = error "no such channel"
  let targetPSD | (isInfixOf "ACC_NO2_Y" channel) == True = "SeisEW_PSD"
                | (isInfixOf "ACC_NO2_X" channel) == True = "SeisNS_PSD"
                | (isInfixOf "ACC_NO2_Z" channel) == True = "SeisZ_PSD"
                | (isInfixOf "MAG_Y" channel) == True = "MagEW_PSD"
                | (isInfixOf "MAG_X" channel) == True = "MagNS_PSD"
                | (isInfixOf "MAG_Z" channel) == True = "MagZ_PSD"
                | (isInfixOf "MIC" channel)   == True = "MIC_PSD"
                | (isInfixOf "REF" channel)   == True = "DAQ_PSD"
                | otherwise = error "no such channel"
  let targetSPE | (isInfixOf "ACC_NO2_Y" channel) == True = "SeisEW_SPE"
                | (isInfixOf "ACC_NO2_X" channel) == True = "SeisNS_SPE"
                | (isInfixOf "ACC_NO2_Z" channel) == True = "SeisZ_SPE"
                | (isInfixOf "MAG_Y" channel) == True = "MagEW_SPE"
                | (isInfixOf "MAG_X" channel) == True = "MagNS_SPE"
                | (isInfixOf "MAG_Z" channel) == True = "MagZ_SPE"
                | (isInfixOf "MIC" channel)   == True = "MIC_SPE"
                | (isInfixOf "REF" channel)   == True = "DAQ_SPE"
                | otherwise = error "no such channel"
  let latestTS | (isInfixOf "ACC_NO2_Y" channel) == True = savePathLatest </> "SeisEW_TS_Latest.png"
               | (isInfixOf "ACC_NO2_X" channel) == True = savePathLatest </> "SeisNS_TS_Latest.png"
               | (isInfixOf "ACC_NO2_Z" channel) == True = savePathLatest </> "SeisZ_TS_Latest.png"
               | (isInfixOf "MAG_Y" channel) == True = savePathLatest </> "MagEW_TS_Latest.png"
               | (isInfixOf "MAG_X" channel) == True = savePathLatest </> "MagNS_TS_Latest.png"
               | (isInfixOf "MAG_Z" channel) == True = savePathLatest </> "MagZ_TS_Latest.png"
               | (isInfixOf "MIC" channel)   == True = savePathLatest </> "MIC_TS_Latest.png"
               | (isInfixOf "REF" channel)   == True = savePathLatest </> "DAQ_TS_Latest.png"
               | otherwise = error "no such channel"
  updateLatestImage plotfname latestTS
  let latestPSD | (isInfixOf "ACC_NO2_Y" channel) == True = savePathLatest </> "SeisEW_PSD_Latest.png"
                | (isInfixOf "ACC_NO2_X" channel) == True = savePathLatest </> "SeisNS_PSD_Latest.png"
                | (isInfixOf "ACC_NO2_Z" channel) == True = savePathLatest </> "SeisZ_PSD_Latest.png"
                | (isInfixOf "MAG_Y" channel) == True = savePathLatest </> "MagEW_PSD_Latest.png"
                | (isInfixOf "MAG_X" channel) == True = savePathLatest </> "MagNS_PSD_Latest.png"
                | (isInfixOf "MAG_Z" channel) == True = savePathLatest </> "MagZ_PSD_Latest.png"
                | (isInfixOf "MIC" channel)   == True = savePathLatest </> "MIC_PSD_Latest.png"
                | (isInfixOf "REF" channel)   == True = savePathLatest </> "DAQ_PSD_Latest.png"
                | otherwise = error "no such channel"
  updateLatestImage plotpsdfname latestPSD
  let latestSPE | (isInfixOf "ACC_NO2_Y" channel) == True = savePathLatest </> "SeisEW_SPE_Latest.png"
                | (isInfixOf "ACC_NO2_X" channel) == True = savePathLatest </> "SeisNS_SPE_Latest.png"
                | (isInfixOf "ACC_NO2_Z" channel) == True = savePathLatest </> "SeisZ_SPE_Latest.png"
                | (isInfixOf "MAG_Y" channel) == True = savePathLatest </> "MagEW_SPE_Latest.png"
                | (isInfixOf "MAG_X" channel) == True = savePathLatest </> "MagNS_SPE_Latest.png"
                | (isInfixOf "MAG_Z" channel) == True = savePathLatest </> "MagZ_SPE_Latest.png"
                | (isInfixOf "MIC" channel)   == True = savePathLatest </> "MIC_SPE_Latest.png"
                | (isInfixOf "REF" channel)   == True = savePathLatest </> "DAQ_SPE_Latest.png"
                | otherwise = error "no such channel"
  updateLatestImage plotspefname latestSPE

  oldlist <- genTargetLatest savePath savePathLatest gTimeS durationS chList
  return $ [(targetTS, latestTS), (targetPSD, latestPSD), (targetSPE, latestSPE)]++oldlist

genLatestImage :: String -> String -> IO()
genLatestImage fromimage toimage = do
  updateLatestImage fromimage toimage

updateWebPage :: [(String, String)] -> String -> String
updateWebPage [] contents = contents
updateWebPage (target:targets) contents = do
  replace (fst target) (snd target) $ updateWebPage targets contents

updateLatestImage' :: String -> String -> IO()
updateLatestImage' original latestLink = do
  fileExists <- fileExist latestLink
  case (fileExists) of
    True -> do
      removeLink latestLink
      createSymbolicLink original latestLink
    False -> createSymbolicLink original latestLink

updateLatestImage :: String -> String -> IO()
updateLatestImage original latest = do
  fileExists <- fileExist latest
  case (fileExists) of
    True -> do
      removeFile latest
      copyFile original latest
    False -> copyFile original latest

