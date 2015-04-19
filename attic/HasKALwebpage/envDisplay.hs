
import HasKAL.PlotUtils.HROOT.PlotGraph
import HasKAL.PlotUtils.HROOT.PlotGraph3D
import HasKAL.FrameUtils.Function (readFrameV)
import HasKAL.FrameUtils.FrameUtils (getChannelList, getGPSTime)
import HasKAL.SpectrumUtils.SpectrumUtils (gwpsdV, gwspectrogramV, Spectrogram)
--import HasKAL.SpectrumUtils.Function (plotFormatedSpectogram)
import Control.Monad (forM_, liftM)
import Data.List (isSuffixOf, isInfixOf)
import Data.Packed.Vector (fromList, subVector, dim)
import System.Environment (getArgs)
import System.Process (rawSystem)
import System.Directory (createDirectoryIfMissing)
import qualified Numeric.LinearAlgebra as NL
import System.PosixCompat.Files (fileExist, removeLink, createSymbolicLink)

main :: IO ()
main = do
  let savePath = "/play/kagra/detector-characterization/attic/HasKALwebpage/env_images/"
  createDirectoryIfMissing True savePath
  args <- getArgs
  case (length args) of
   0 -> error "Usage: envDisplay frameFilename"
   _ -> allChannelPlot savePath (args !! 0)


allChannelPlot :: String -- save path of png file ("/path/to/dir/")
               -> String -- frame file "/path/to/data/hoge.gwf"
               -> IO ()
allChannelPlot savePath filename = do
  let savePathLatest = (dropLastSlash savePath)++"_latest/"
  createDirectoryIfMissing True savePathLatest
  gTimeS <- liftM (show.fst) $ getGPSTime filename
--  chList <- liftM ((take 8).filter ((==4).length).fst) $ getChannelList filename
  chList' <- getChannelList filename
  let chList = take 8 [(channel, fs)|(channel, fs)<-chList', (isSuffixOf "_FLOOR" channel)|| (channel=="K1:PEM-EX_REF")]
  forM_ chList $ \(channel, fs) -> do
    let plotfname = savePath++channel++"_TS-"++gTimeS++".jpg"
        plotpsdfname = savePath++channel++"_PSD-"++gTimeS++".jpg"
        plotspefname = savePath++channel++"_SPE-"++gTimeS++".jpg"
    xs <- readFrameV channel filename
    case (isSuffixOf "-RAW" channel) of
     True -> do
       plotV Linear Line 1 BLUE ("[s]", "ADC Count") 0.05 channel plotfname
         ((0,0),(0,0)) $ (fromList [1/fs,2/fs..32], xs)
     False -> do
       plotV Linear Line 1 BLUE ("[s]", "[V]") 0.05 channel plotfname
         ((0,0),(0,0)) $ (fromList [1/fs,2/fs..32], xs)
       let (ys, zs) = gwpsdV xs (dim xs) fs
       plotV LogXY Line 1 BLUE ("[Hz]", "[V/rHz]") 0.04 channel plotpsdfname
         ((0,0),(0,0)) (subVector 0 (dim ys `div` 2 - 1) ys, subVector 0 (dim zs `div` 2 - 1) (sqrt zs))
       spectrogramM LogYZ COLZ " " (channel) plotspefname
         $ setRange 3 1024 $ gwspectrogramV 0 (truncate fs) fs xs
       let plotfnameLatest | (isInfixOf "ACC_NO2_Y" channel) == True = savePathLatest++"SeisEW_TS_Latest.jpg"
                           | (isInfixOf "ACC_NO2_X" channel) == True = savePathLatest++"SeisNS_TS_Latest.jpg"
                           | (isInfixOf "ACC_NO2_Z" channel) == True = savePathLatest++"SeisZ_TS_Latest.jpg"
                           | (isInfixOf "MAG_Y" channel) == True = savePathLatest++"MagEW_TS_Latest.jpg"
                           | (isInfixOf "MAG_X" channel) == True = savePathLatest++"MagNS_TS_Latest.jpg"
                           | (isInfixOf "MAG_Z" channel) == True = savePathLatest++"MagZ_TS_Latest.jpg"
                           | (isInfixOf "MIC" channel)   == True = savePathLatest++"MIC_TS_Latest.jpg"
                           | (isInfixOf "REF" channel)   == True = savePathLatest++"DAQ_TS_Latest.jpg"
                           | otherwise = error "no such channel"
       --_<- rawSystem "ln" ["-fs", plotfname, plotfnameLatest]
       updateLatestImage plotfname plotfnameLatest

       let plotpsdfnameLatest | (isInfixOf "ACC_NO2_Y" channel) == True = savePathLatest++"SeisEW_PSD_Latest.jpg"
                              | (isInfixOf "ACC_NO2_X" channel) == True = savePathLatest++"SeisNS_PSD_Latest.jpg"
                              | (isInfixOf "ACC_NO2_Z" channel) == True = savePathLatest++"SeisZ_PSD_Latest.jpg"
                              | (isInfixOf "MAG_Y" channel) == True = savePathLatest++"MagEW_PSD_Latest.jpg"
                              | (isInfixOf "MAG_X" channel) == True = savePathLatest++"MagNS_PSD_Latest.jpg"
                              | (isInfixOf "MAG_Z" channel) == True = savePathLatest++"MagZ_PSD_Latest.jpg"
                              | (isInfixOf "MIC" channel)   == True = savePathLatest++"MIC_PSD_Latest.jpg"
                              | (isInfixOf "REF" channel)   == True = savePathLatest++"DAQ_PSD_Latest.jpg"
                              | otherwise = error "no such channel"
       updateLatestImage plotpsdfname plotpsdfnameLatest

       let plotspefnameLatest | (isInfixOf "ACC_NO2_Y" channel) == True = savePathLatest++"SeisEW_SPE_Latest.jpg"
                              | (isInfixOf "ACC_NO2_X" channel) == True = savePathLatest++"SeisNS_SPE_Latest.jpg"
                              | (isInfixOf "ACC_NO2_Z" channel) == True = savePathLatest++"SeisZ_SPE_Latest.jpg"
                              | (isInfixOf "MAG_Y" channel) == True = savePathLatest++"MagEW_SPE_Latest.jpg"
                              | (isInfixOf "MAG_X" channel) == True = savePathLatest++"MagNS_SPE_Latest.jpg"
                              | (isInfixOf "MAG_Z" channel) == True = savePathLatest++"MagZ_SPE_Latest.jpg"
                              | (isInfixOf "MIC" channel)   == True = savePathLatest++"MIC_SPE_Latest.jpg"
                              | (isInfixOf "REF" channel)   == True = savePathLatest++"DAQ_SPE_Latest.jpg"
                              | otherwise = error "no such channel"
       updateLatestImage plotspefname plotspefnameLatest


updateLatestImage :: String -> String -> IO()
updateLatestImage original latestLink = do
  fileExists <- fileExist latestLink
  case (fileExists) of
    True -> do
      removeLink latestLink
      createSymbolicLink original latestLink
    False -> createSymbolicLink original latestLink


dropLastSlash :: String -> String
dropLastSlash s = take (length s -1) s


setRange :: Double -> Double -> Spectrogram -> Spectrogram
setRange flow fhigh spec = do
  let (tv, fv, p) = spec
      flowIndex = head $ NL.find (>=flow) fv
      nrow = NL.rows p
      (tv', fv', p') = (tv, subVector flowIndex (nrow-flowIndex) fv, NL.dropRows flowIndex p)
      fhighIndex = last $ NL.find (<=fhigh) fv'
  (tv', subVector 0 fhighIndex fv', NL.takeRows fhighIndex p')


