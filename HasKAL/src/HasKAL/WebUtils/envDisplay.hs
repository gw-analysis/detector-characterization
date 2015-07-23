
import HasKAL.PlotUtils.HROOT.PlotGraph
import HasKAL.PlotUtils.HROOT.PlotGraph3D
import HasKAL.FrameUtils.Function (readFrameV)
import HasKAL.FrameUtils.FrameUtils (getChannelList, getGPSTime)
import HasKAL.FrameUtils.FileManipulation (extractDataLengthfromFilename)
import HasKAL.SpectrumUtils.SpectrumUtils (gwpsdV, gwspectrogramV, Spectrogram)
import HasKAL.TimeUtils.GPSfunction (gps2time)

import Control.Monad (forM_, liftM)
import Data.List (isSuffixOf, isInfixOf)
import Data.Maybe (fromMaybe)
import Data.Packed.Vector (fromList, subVector, dim)
import Data.String.Utils (replace)
import System.Directory (createDirectoryIfMissing, copyFile, removeFile)
import System.Environment (getArgs)
import qualified Numeric.LinearAlgebra as NL
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import System.PosixCompat.Files (fileExist, removeLink, createSymbolicLink)


main :: IO ()
main = do
  --let homePath = "/play/kagra/detector-characterization/attic/HasKALwebpage"
  --currentDir <- getCurrentDirectory
  let savePath = "env_images"
  createDirectoryIfMissing True savePath
  args <- getArgs
  case length args of
   0 -> error "Usage: envDisplay frameFilename"
   2 -> allChannelPlot (head args) (args!!1)


allChannelPlot :: String -- home path ("/path/to/dir/")
               -> String -- frame file "/path/to/data/hoge.gwf"
               -> IO ()
allChannelPlot homePath filename = do
  let savePath = homePath </> "env_images"
      savePathLatest = savePath++"_latest"
  createDirectoryIfMissing True savePath
  createDirectoryIfMissing True savePathLatest
  maybegps <- getGPSTime filename
  let (gTimeS, gTimeN, durationS) = fromMaybe (error " not gps infomation in the file") maybegps
  maybech <- getChannelList filename
  let chList' = fromMaybe (error " not channel in the file.") maybech
  let chList = take 8 [(channel, fs)|(channel, fs)<-chList', (isSuffixOf "_FLOOR" channel)|| (channel=="K1:PEM-EX_REF")]
  forM_ chList $ \(channel, fs) -> do
    let plotfname = savePath </> channel++"_TS-"++gTimeS++"-"++durationS++".png"
        plotpsdfname = savePath </> channel++"_PSD-"++gTimeS++"-"++durationS++".png"
        plotspefname = savePath </> channel++"_SPE-"++gTimeS++"-"++durationS++".png"
    maybexs <- readFrameV channel filename
    let xs = fromMaybe (error " no data in the file.") maybexs
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
         ((0.0,0.0),(3.0,1024.0)) $ gwspectrogramV 0 (truncate fs) fs xs
-- | generate an event display page
  contents <- readFile $ homePath </> "template.html"
  updatetarget <- genTargetLatest savePath savePathLatest gTimeS durationS chList

  let updatedContents = updateWebPage updatetarget $ updateWebPage (updateTime "GPS_TIME" (read gTimeS :: Integer)) contents
      newhtml = homePath </> "index.html"
  writeFile newhtml updatedContents


updateTime :: String -> Integer -> [(String, String)]
updateTime timePlaceHolder gpstime = [(timePlaceHolder, gps2time gpstime)]

genTargetLatest :: String -> String -> String -> String -> [(String, Double)] -> IO [(String, String)]
genTargetLatest _ _ _ _ [] = return []
genTargetLatest savePath savePathLatest gTimeS durationS (ch:chList) = do
  let (channel, _) = ch
      plotfname = savePath </> channel++"_TS-"++gTimeS++"-"++durationS++".png"
      plotpsdfname = savePath </> channel++"_PSD-"++gTimeS++"-"++durationS++".png"
      plotspefname = savePath </> channel++"_SPE-"++gTimeS++"-"++durationS++".png"


  let targetTS | isInfixOf "ACC_NO2_Y" channel = "SeisEW_TS"
               | isInfixOf "ACC_NO2_X" channel = "SeisNS_TS"
               | isInfixOf "ACC_NO2_Z" channel = "SeisZ_TS"
               | isInfixOf "MAG_Y" channel = "MagEW_TS"
               | isInfixOf "MAG_X" channel = "MagNS_TS"
               | isInfixOf "MAG_Z" channel = "MagZ_TS"
               | isInfixOf "MIC" channel = "MIC_TS"
               | isInfixOf "REF" channel = "DAQ_TS"
               | otherwise = error "no such channel"
  let targetPSD | isInfixOf "ACC_NO2_Y" channel = "SeisEW_PSD"
                | isInfixOf "ACC_NO2_X" channel = "SeisNS_PSD"
                | isInfixOf "ACC_NO2_Z" channel = "SeisZ_PSD"
                | isInfixOf "MAG_Y" channel = "MagEW_PSD"
                | isInfixOf "MAG_X" channel = "MagNS_PSD"
                | isInfixOf "MAG_Z" channel = "MagZ_PSD"
                | isInfixOf "MIC" channel = "MIC_PSD"
                | isInfixOf "REF" channel = "DAQ_PSD"
                | otherwise = error "no such channel"
  let targetSPE | isInfixOf "ACC_NO2_Y" channel = "SeisEW_SPE"
                | isInfixOf "ACC_NO2_X" channel = "SeisNS_SPE"
                | isInfixOf "ACC_NO2_Z" channel = "SeisZ_SPE"
                | isInfixOf "MAG_Y" channel = "MagEW_SPE"
                | isInfixOf "MAG_X" channel = "MagNS_SPE"
                | isInfixOf "MAG_Z" channel = "MagZ_SPE"
                | isInfixOf "MIC" channel = "MIC_SPE"
                | isInfixOf "REF" channel = "DAQ_SPE"
                | otherwise = error "no such channel"

  let latestTS | isInfixOf "ACC_NO2_Y" channel = savePathLatest </> "SeisEW_TS_Latest.png"
               | isInfixOf "ACC_NO2_X" channel = savePathLatest </> "SeisNS_TS_Latest.png"
               | isInfixOf "ACC_NO2_Z" channel = savePathLatest </> "SeisZ_TS_Latest.png"
               | isInfixOf "MAG_Y" channel = savePathLatest </> "MagEW_TS_Latest.png"
               | isInfixOf "MAG_X" channel = savePathLatest </> "MagNS_TS_Latest.png"
               | isInfixOf "MAG_Z" channel = savePathLatest </> "MagZ_TS_Latest.png"
               | isInfixOf "MIC" channel = savePathLatest </> "MIC_TS_Latest.png"
               | isInfixOf "REF" channel = savePathLatest </> "DAQ_TS_Latest.png"
               | otherwise = error "no such channel"
  updateLatestImage plotfname latestTS

  let latestPSD | isInfixOf "ACC_NO2_Y" channel = savePathLatest </> "SeisEW_PSD_Latest.png"
                | isInfixOf "ACC_NO2_X" channel = savePathLatest </> "SeisNS_PSD_Latest.png"
                | isInfixOf "ACC_NO2_Z" channel = savePathLatest </> "SeisZ_PSD_Latest.png"
                | isInfixOf "MAG_Y" channel = savePathLatest </> "MagEW_PSD_Latest.png"
                | isInfixOf "MAG_X" channel = savePathLatest </> "MagNS_PSD_Latest.png"
                | isInfixOf "MAG_Z" channel = savePathLatest </> "MagZ_PSD_Latest.png"
                | isInfixOf "MIC" channel = savePathLatest </> "MIC_PSD_Latest.png"
                | isInfixOf "REF" channel = savePathLatest </> "DAQ_PSD_Latest.png"
                | otherwise = error "no such channel"
  updateLatestImage plotpsdfname latestPSD
  let latestSPE | isInfixOf "ACC_NO2_Y" channel = savePathLatest </> "SeisEW_SPE_Latest.png"
                | isInfixOf "ACC_NO2_X" channel = savePathLatest </> "SeisNS_SPE_Latest.png"
                | isInfixOf "ACC_NO2_Z" channel = savePathLatest </> "SeisZ_SPE_Latest.png"
                | isInfixOf "MAG_Y" channel = savePathLatest </> "MagEW_SPE_Latest.png"
                | isInfixOf "MAG_X" channel = savePathLatest </> "MagNS_SPE_Latest.png"
                | isInfixOf "MAG_Z" channel = savePathLatest </> "MagZ_SPE_Latest.png"
                | isInfixOf "MIC" channel = savePathLatest </> "MIC_SPE_Latest.png"
                | isInfixOf "REF" channel = savePathLatest </> "DAQ_SPE_Latest.png"
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
  case fileExists of
    True -> do
      removeLink latestLink
      createSymbolicLink original latestLink
    False -> createSymbolicLink original latestLink

updateLatestImage :: String -> String -> IO()
updateLatestImage original latest = do
  fileExists <- fileExist latest
  case fileExists of
    True -> do
      removeFile latest
      copyFile original latest
    False -> copyFile original latest


setRange :: Double -> Double -> Spectrogram -> Spectrogram
setRange flow fhigh spec = do
  let (tv, fv, p) = spec
      flowIndex = head $ NL.find (>=flow) fv
      nrow = NL.rows p
      (tv', fv', p') = (tv, subVector flowIndex (nrow-flowIndex) fv, NL.dropRows flowIndex p)
      fhighIndex = last $ NL.find (<=fhigh) fv'
  (tv', subVector 0 fhighIndex fv', NL.takeRows fhighIndex p')


