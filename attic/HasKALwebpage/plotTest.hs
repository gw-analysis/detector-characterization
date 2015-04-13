{-******************************************
  *     File Name: plotTest.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2015/04/13 19:04:04
  *******************************************-}

import HasKAL.PlotUtils.HROOT.PlotGraph
import HasKAL.FrameUtils.Function (readFrameV)
import HasKAL.FrameUtils.FrameUtils (getChannelList, getGPSTime)
import HasKAL.SpectrumUtils.SpectrumUtils (gwpsdV)
import Control.Monad (forM_, liftM)
import Data.List (isSuffixOf)
import Data.Packed.Vector (fromList, subVector, dim)
import System.Environment (getArgs)

main :: IO ()
main = do
  let savePath = "/Users/justice/Physics/Data/"
  args <- getArgs
  case (length args) of
   0 -> error "Usage: plotTest filename"
   _ -> allChannelPlot savePath (args !! 0)


allChannelPlot :: String -- save path of png file ("/path/to/dir/")
               -> String -- frame file "/path/to/data/hoge.gwf"
               -> IO ()
allChannelPlot savePath filename = do
  gTimeS <- liftM (show.fst) $ getGPSTime filename
  chList <- liftM (take 8) $ getChannelList filename
  forM_ chList $ \(channel, fs) -> do
    xs <- readFrameV channel filename
    case (isSuffixOf "-RAW" channel) of
     True -> do
       plotV Linear Line 1 BLUE ("[sec]", "ADC Count") 0.04 channel (savePath++channel++"-"++gTimeS++".png")
         ((0,0),(0,0)) $ (fromList [1/fs,2/fs..32], xs)
     False -> do
       plotV Linear Line 1 BLUE ("[sec]", "[V]") 0.04 channel (savePath++channel++"-"++gTimeS++".png")
         ((0,0),(0,0)) $ (fromList [1/fs,2/fs..32], xs)
       let (ys, zs) = gwpsdV xs (dim xs) fs
       plotV LogXY Line 1 BLUE ("[Hz]", "[V^2/Hz]") 0.04 channel (savePath++channel++"_FFT-"++gTimeS++".png")
         ((0,0),(0,0)) (subVector 0 (dim ys `div` 2 - 1) ys, subVector 0 (dim zs `div` 2 - 1) zs)
         

