


import GlitchMon.GlitchMonTime
import GlitchMon.GlitchParam
import GlitchMon.PipelineFunction
import HasKAL.DataBaseUtils.FrameFull.Data

main = do
  let fs = 4096
  let param = GlitchParam
               { segmentLength = 32
               , channel = "K1:LSC-MICH_CTRL_CAL_OUT_DQ"
               , chunklen = 4.0 :: Double --[s]
               , samplingFrequency = fs
             -- * whitening
               , refpsdlen = 1.0 :: Double --[s]
               , whtfiltordr = 1000
               , whtCoeff = []
             -- * t-f expression
               , nfrequency = 0.2
               , ntimeSlide = 0.03
             -- * clustering
               , cutoffFractionTFT = 0.5
               , cutoffFractionTFF = 0.5
               , cutoffFreq = 10
               , clusterThres = 10.0
               , celement = basePixel9
               , minimumClusterNum = 2
             -- * clean data finder
               , cdfInterval = 32
               , cdfparameter = cdfp
               , cgps = Nothing
             -- * temporary data
--               , refpsd =
--               , refwave =
               , reftime = 0
               -- * for debug
               , debugmode = 1
                 }

      cdfp = CDFParam
              { cdf'samplingFrequency = fs
              , cdf'cutoffFrequencyLow = 10
              , cdf'cutoffFrequencyHigh = 500
              , cdf'blockSize = 50
              , cdf'fftSize = nfrequency param
              , cdf'chunkSize = chunklen param
              }


  runGlitchMonTime param (channel param) "./dat/test.lst"



