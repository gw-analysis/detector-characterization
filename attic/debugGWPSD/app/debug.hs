--module Main where

import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import qualified Data.Vector.Storable as V
import HasKAL.PlotUtils.HROOT.PlotGraph
import HasKAL.SpectrumUtils.SpectrumUtils
import HasKAL.FrameUtils.Function (readFrameV)
import HasKAL.FrameUtils.FrameUtils(getChannelList)
import HasKAL.SignalProcessingUtils.WindowType
import Data.Time
import Control.DeepSeq (deepseq)


--main :: IO ()
main = do
  let fname = "/home/kazu/work/data/gw150914/H-H1_LOSC_4_V1-1126259446-32.gwf"
  ch' <- getChannelList fname
  let (ch, fs) = head [ (x, y) | (x, y) <- (fromMaybe (error "not valid data") ch'), isInfixOf "STRAIN" x]
  x <- readFrameV ch fname
  let dat = fromMaybe (error "not valid data") x
      nfft = truncate fs
  t1 <- getCurrentTime
  let psd = gwOnesidedPSDV dat nfft fs
  psd `deepseq` return ()
  t2 <- getCurrentTime

  plotV LogXY Line 1 RED ("[Hz]",  "[1/Hz^1/2]") 0.05 ch "gwpsd.png" ((0, 0), (0, 0)) psd
  print $ diffUTCTime t2 t1



