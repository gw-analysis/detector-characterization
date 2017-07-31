

import Data.Maybe (fromMaybe)
import HasKAL.IOUtils.Function (loadASCIIdataCV)
import HasKAL.MonitorUtils.GlitchMon.GlitchMonDAT
import HasKAL.MonitorUtils.GlitchMon.GlitchParam
import HasKAL.MonitorUtils.GlitchMon.PipelineFunction
import HasKAL.DataBaseUtils.FrameFull.Data
import HasKAL.FrameUtils.FrameUtils (getSamplingFrequency)
import System.Environment ( getArgs)
import HasKAL.Misc.ConfFile (readConfFile)

main = do
  (conffile, fsorig', startGPStime', datfile) <- getArgs >>= \args -> case (length args) of
    4 -> return (head args, args!!1, args!!2, args!!3)
    _ -> error "Usage runGlitchMonFile conffile fs startGPStime datfile"
  ([sl, ch, sf, tl, wFR, wM, nf, nTS, cTFT, cTFF, cF, cT, minN, nN, maxN, cdfI], [qs])
    <- readConfFile conffile ["segmentLength", "channel", "samplingFrequency",
        "traindatlen", "whnFrequencyResolution", "whnMethod", "nfrequency", "ntimeSlide",
        "cutoffFractionTFT", "cutoffFractionTFF", "cutoffFreq", "clusterThres",
        "minimumClusterNum", "nNeighbor", "maxNtrigg", "cdfInterval"] [""]
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
               , nfrequency = read nf :: Double
               , ntimeSlide = read nTS :: Double
             -- * clustering
               , cutoffFractionTFT = read cTFT :: Double
               , cutoffFractionTFF = read cTFF :: Double
               , cutoffFreq = read cF :: Double
               , clusterThres = read cT :: Double
               , celement = basePixel9
               , minimumClusterNum = read minN :: Int
               , nNeighbor = read nN :: Int
               , maxNtrigg = read maxN :: Int
             -- * clean data finder
               , cdfInterval = read cdfI :: Int
               , cdfparameter = cdfp
               , cgps = Nothing
               , reftime = 0
             -- * for debug
               , debugmode = [DS, DC, TF, CL]
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

  let v = head $ loadASCIIdataCV datfile
  runGlitchMonDAT param (channel param) startGPStime v
--"/home/kazu/work/data/gw150914/H-H1_LOSC_4_V1-1126259446-32.gwf"
--  runGlitchMonFile param (channel param) "/data/kagra/raw/full/K-K1_C-1144374208-32.gwf"
