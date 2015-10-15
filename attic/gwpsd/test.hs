

-- ghc -threaded -rtsopts -fllvm test.hs
-- HasKAL/PlotUtils/HROOT/AppendFunction.cc -lFrame
-- -I/usr/local/include/root
--
--  ./test +RTS -N3

import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
--import TESTBEDGPWPSD(gwOnesidedPSDWelchP1, gwOnesidedPSDWelchP2)
import TESTBEDGPWPSD(gwpsdV)
import HasKAL.PlotUtils.HROOT.PlotGraph
--import HasKAL.SpectrumUtils.SpectrumUtils(gwpsdV)
import HasKAL.FrameUtils.Function (readFrameV)
import HasKAL.FrameUtils.FrameUtils(getChannelList)
import HasKAL.SignalProcessingUtils.WindowType
import Data.Time
import Control.DeepSeq (deepseq)
--import SpectrumUtilsRefac (gwpsdV, gwpsdVP)



main = do
  let fname = "L-L1_LOSC_4_V1-855318528-4096.gwf"
  ch' <- getChannelList fname
  let (ch, fs) = head [ (x, y) | (x, y) <- (fromMaybe (error "not valid data") ch'), isInfixOf "STRAIN" x]
  x <- readFrameV ch fname
  let dat = fromMaybe (error "not valid data") x
  dat `deepseq` return ()
  let nfft = 10*truncate fs

  t1 <- getCurrentTime
--  let psd = gwOnesidedPSDWelchP2 dat nfft fs Hann
  let psd = gwpsdV dat nfft fs
  psd `deepseq` return ()
  t2 <- getCurrentTime

  plotV LogXY Line 1 RED ("[Hz]",  "[1/Hz^1/2]") 0.04 ch "new_gwpsd.eps" ((0, 0), (0, 0)) psd
  print $ diffUTCTime t2 t1




