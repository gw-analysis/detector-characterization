
import Control.Monad (forM_,  liftM)
import Data.List (isSuffixOf, isInfixOf)
import Data.Maybe (fromMaybe)
import Data.Packed.Vector (fromList, subVector, dim)
import Data.String.Utils (replace)
import HasKAL.FrameUtils.Function (readFrameFromGPS'V)
import HasKAL.FrameUtils.FrameUtils (getChannelList, getGPSTime)
import HasKAL.FrameUtils.PickUpFileName (pickUpFileNameinFile)
import HasKAL.PlotUtils.HROOT.PlotGraph
import HasKAL.PlotUtils.HROOT.PlotGraph3D
import HasKAL.SpectrumUtils.SpectrumUtils (gwpsdV, gwspectrogramV, Spectrogram)
import HasKAL.TimeUtils.GPSfunction (gps2time)
import qualified Numeric.LinearAlgebra as NL
import System.Directory (createDirectoryIfMissing, copyFile, removeFile)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.PosixCompat.Files (fileExist)


main :: IO ()
main = do
  let homePath= "/play/kagra/detector-characterization/attic/HasKALwebpage"
--      savePath = homePath </> "env_images"
  args <- getArgs
  let startgps = read (head args) :: Integer
      duration = read (args !! 1) :: Integer
      cachefile = args !! 2
  case length args of
   3 -> allChannelPlot homePath startgps duration cachefile
   _ -> error "Usage: envDisplay GPStime Duration framecache"


allChannelPlot :: String -- save path of.png file ("/path/to/dir/")
               -> Integer
               -> Integer
               -> String -- framecache file "/path/to/data/framecache"
               -> IO ()
allChannelPlot homePath startGPS duration fcache = do
  let gTimeS = show startGPS
      durationS = show duration
      savePath = homePath </> "env_images"
  createDirectoryIfMissing True savePath
--  chList <- liftM ((take 8).filter ((==4).length).fst) $ getChannelList filename
  let datafiles = pickUpFileNameinFile startGPS (startGPS + duration - 1) fcache
  maybech <- getChannelList (head datafiles)
  let chList' = fromMaybe (error "channel not found in the file") maybech
  let chList = take 8 [(channel, fs)|(channel, fs)<-chList', (isSuffixOf "_FLOOR" channel)|| (channel=="K1:PEM-EX_REF")]
  forM_ chList $ \(channel, fs) -> do
    let plotfname = savePath </> channel++"_TS-"++gTimeS++"-"++durationS++".png"
        plotpsdfname = savePath </> channel++"_PSD-"++gTimeS++"-"++durationS++".png"
        plotspefname = savePath </> channel++"_SPE-"++gTimeS++"-"++durationS++".png"
    maybexs <- readFrameFromGPS'V startGPS duration channel fcache
    let xs = fromMaybe (error "data not found in the file") maybexs
    case (isSuffixOf "-RAW" channel) of
     True -> do
       plotV Linear Line 1 BLUE ("[s]", "ADC Count") 0.05 channel plotfname
         ((0,0),(0,0)) $ (fromList [1/fs,2/fs..(fromInteger duration)], xs)
     False -> do
       plotV Linear Line 1 BLUE ("[s]", "[V]") 0.05 channel plotfname
         ((0,0),(0,0)) $ (fromList [1/fs,2/fs..(fromInteger duration)], xs)
       let (ys, zs) = gwpsdV xs (truncate fs) fs
       plotV LogXY Line 1 BLUE ("[Hz]", "[V/rHz]") 0.05 channel plotpsdfname
         ((0,0),(0,0)) (subVector 0 (dim ys `div` 2 - 1) ys, subVector 0 (dim zs `div` 2 - 1) (sqrt zs))
       spectrogramM LogYZ COLZ " " channel plotspefname
         ((0.0,0.0),(3.0,1024.0)) $ gwspectrogramV 0 (truncate fs) fs xs

-- | generate an event display page
  contents <- readFile $ homePath </> "template.html"
  updatetarget <- genTarget savePath gTimeS durationS chList
  let updatedContents = updateWebPage updatetarget $ updateWebPage (updateTime "GPS_TIME" startGPS) contents
      newhtml = homePath </> "index-"++(show startGPS)++"-"++(show duration)++".html"
  writeFile newhtml updatedContents

updateTime :: String -> Integer -> [(String, String)]
updateTime timePlaceHolder gpstime = [(timePlaceHolder, gps2time gpstime)]

genTarget :: String -> String -> String -> [(String, Double)] -> IO [(String, String)]
genTarget _ _ _ [] = return []
genTarget savePath gTimeS durationS (ch:chList) = do
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
  updateLatestImage plotfname targetTS
  let targetPSD | (isInfixOf "ACC_NO2_Y" channel) == True = "SeisEW_PSD"
                | (isInfixOf "ACC_NO2_X" channel) == True = "SeisNS_PSD"
                | (isInfixOf "ACC_NO2_Z" channel) == True = "SeisZ_PSD"
                | (isInfixOf "MAG_Y" channel) == True = "MagEW_PSD"
                | (isInfixOf "MAG_X" channel) == True = "MagNS_PSD"
                | (isInfixOf "MAG_Z" channel) == True = "MagZ_PSD"
                | (isInfixOf "MIC" channel)   == True = "MIC_PSD"
                | (isInfixOf "REF" channel)   == True = "DAQ_PSD"
                | otherwise = error "no such channel"
  updateLatestImage plotpsdfname targetPSD
  let targetSPE | (isInfixOf "ACC_NO2_Y" channel) == True = "SeisEW_SPE"
                | (isInfixOf "ACC_NO2_X" channel) == True = "SeisNS_SPE"
                | (isInfixOf "ACC_NO2_Z" channel) == True = "SeisZ_SPE"
                | (isInfixOf "MAG_Y" channel) == True = "MagEW_SPE"
                | (isInfixOf "MAG_X" channel) == True = "MagNS_SPE"
                | (isInfixOf "MAG_Z" channel) == True = "MagZ_SPE"
                | (isInfixOf "MIC" channel)   == True = "MIC_SPE"
                | (isInfixOf "REF" channel)   == True = "DAQ_SPE"
                | otherwise = error "no such channel"
  updateLatestImage plotspefname targetSPE
  oldList <- genTarget savePath gTimeS durationS chList
  return $ [(targetTS, plotfname), (targetPSD, plotpsdfname), (targetSPE, plotspefname)]++oldList

updateLatestImage :: String -> String -> IO()
updateLatestImage original latest = do
  fileExists <- fileExist latest
  case (fileExists) of
    True -> do
      removeFile latest
      copyFile original latest
    False -> copyFile original latest

updateWebPage :: [(String, String)] -> String -> String
updateWebPage [] contents = contents
updateWebPage (target:targets) contents = do
  replace (fst target) (snd target) $ updateWebPage targets contents

setRange :: Double -> Double -> Spectrogram -> Spectrogram
setRange flow fhigh spec = do
  let (tv, fv, p) = spec
      flowIndex = head $ NL.find (>=flow) fv
      nrow = NL.rows p
      (tv', fv', p') = (tv, subVector flowIndex (nrow-flowIndex) fv, NL.dropRows flowIndex p)
      fhighIndex = last $ NL.find (<=fhigh) fv'
  (tv', subVector 0 fhighIndex fv', NL.takeRows fhighIndex p')


