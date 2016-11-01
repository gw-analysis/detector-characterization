
import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import GlitchMon.GlitchMonTime
import GlitchMon.GlitchParam
import GlitchMon.PipelineFunction
import HasKAL.DataBaseUtils.FrameFull.Data
import HasKAL.DataBaseUtils.FrameFull.Function (kagraDataPoint)
import HasKAL.FrameUtils.FrameUtils (getSamplingFrequency)
import System.Environment (getArgs)

main = do
  (chname, fname) <- getArgs >>= \args -> case (length args) of
    2 -> return (head args, args!!1)
    _ -> error "Usage: runGlitchMonTime chname fname"
  let gpslist = selectSegment 32 fname
      gps = fromIntegral $ fst . head $ gpslist :: Int32
  tmpfnames <- fmap (fromMaybe (error "fs not found.")) $ kagraDataPoint gps chname
  fsorig <- fmap (fromMaybe (error "fs not found.")) $ getSamplingFrequency (head tmpfnames) chname
--  print fsorig
  let dfactor = 4.0
  let fs =  fromIntegral $ floor (fsorig / dfactor)
  let param = GlitchParam
               { segmentLength = 32
               , channel = chname -- "K1:LSC-MICH_CTRL_CAL_OUT_DQ"
               , samplingFrequency = fs
             -- * whitening
               , traindatlen = 30.0 :: Double --[s]
               , whnFrequencyResolution = 4.0 :: Double --[Hz]
               , whtCoeff = []
               , whnMethod = FrequencyDomain
             -- * t-f expression
               , nfrequency = 0.2
               , ntimeSlide = 0.03
             -- * clustering
               , cutoffFractionTFT = 0.5
               , cutoffFractionTFF = 0.5
               , cutoffFreq = 30
               , clusterThres = 10.0
               , celement = basePixel9
               , minimumClusterNum = 6
               , nNeighbor = 3
               , maxNtrigg = 50
             -- * clean data finder
               , cdfInterval = 32
               , cdfparameter = cdfp
               , cgps = Nothing
             -- * temporary data
--               , refpsd =
--               , refwave =
               , reftime = 0
               -- * for debug
               , debugmode =[] 
               , debugDir = "debug"
                 }

      cdfp = CDFParam
              { cdf'samplingFrequency = fs
              , cdf'cutoffFrequencyLow = 10
              , cdf'cutoffFrequencyHigh = 500
              , cdf'blockSize = 50
              , cdf'fftSize = nfrequency param
              , cdf'chunkSize = traindatlen param
              }


  runGlitchMonTime param (channel param) fname
--  runGlitchMonTime param (channel param) "./dat/test3.lst"


