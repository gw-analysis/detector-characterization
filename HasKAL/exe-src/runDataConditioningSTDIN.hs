

import Data.Maybe (fromMaybe)
import qualified Data.Vector.Storable as VS
import HasKAL.IOUtils.Function (stdin2vec)
import HasKAL.MonitorUtils.GlitchMon.GlitchMonDAT
import HasKAL.MonitorUtils.GlitchMon.GlitchParam
import HasKAL.MonitorUtils.GlitchMon.PipelineFunction
import HasKAL.DataBaseUtils.FrameFull.Data
import HasKAL.FrameUtils.FrameUtils (getSamplingFrequency)
import qualified HasKAL.WaveUtils.Data as W (WaveData(..), vec2wave)
import System.Environment ( getArgs)
import HasKAL.Misc.ConfFile (readConfFile)
import System.IO (stdout, hPutStrLn)


main = do
  (conffile, fsorig', startGPStime') <- getArgs >>= \args -> case (length args) of
    3 -> return (head args, args!!1, args!!2)
    _ -> error "Usage: runDataConditioningSTDIN conffile fs startGPStime STDIN"
  ([sl, ch, sf, tl, wFR, wM], [])
    <- readConfFile conffile ["segmentLength", "channel", "samplingFrequency",
        "traindatlen", "whnFrequencyResolution", "whnMethod"] []
  let fsorig = read fsorig' :: Double
  let startGPStime = read startGPStime' :: Double

  let param = GlitchParam
               { segmentLength = read sl :: Int
--               , channel = "K1:LSC-MICH_CTRL_CAL_OUT_DQ"
               , channel = ch --"H1:LOSC-STRAIN"
               , samplingFrequency = read sf :: Double
             -- * whitening
               , traindatlen = read tl :: Double -- [s] must be > 2 * fs/whnFrequencyResolution
               , whnFrequencyResolution = read wFR :: Double -- [Hz]
               , whtCoeff = []
               , whnMethod = read wM :: WhnMethod
             -- * t-f expression
               , nfrequency = 0 :: Double
               , ntimeSlide = 0 :: Double
             -- * clustering
               , cutoffFractionTFT = 0 :: Double
               , cutoffFractionTFF = 0 :: Double
               , cutoffFreq = 0 :: Double
               , clusterThres = 0 :: Double
               , celement = basePixel9
               , minimumClusterNum = 0 :: Int
               , nNeighbor = 0:: Int
               , maxNtrigg = 0 :: Int
             -- * clean data finder
               , cdfInterval = 0 :: Int
               , cdfparameter = cdfp
               , cgps = Nothing
               , reftime = 0
             -- * for debug
               , debugmode = [DS, DC, WH, TF, CL]
               , debugDir = "production"
               }
      cdfp = CDFParam
              { cdf'samplingFrequency = read sf :: Double
              , cdf'cutoffFrequencyLow = 10
              , cdf'cutoffFrequencyHigh = 500
              , cdf'blockSize = 50
              , cdf'fftSize = nfrequency param
              , cdf'chunkSize = traindatlen param
              }

  v <- stdin2vec
  let w = W.vec2wave fsorig startGPStime v
  maybeWaveData <- runDataConditioningDAT param (channel param) w
  case maybeWaveData of
    Nothing -> error "Usage: runDataConditioningSTDIN conffile fs startGPStime STDIN"
    Just w -> mapM_ (\y -> hPutStrLn stdout $ show y) (VS.toList (W.gwdata w))

--"/home/kazu/work/data/gw150914/H-H1_LOSC_4_V1-1126259446-32.gwf"
--  runGlitchMonFile param (channel param) "/data/kagra/raw/full/K-K1_C-1144374208-32.gwf"
