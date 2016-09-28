


import GlitchMon.GlitchMonFile
import GlitchMon.GlitchParam
import GlitchMon.PipelineFunction
import HasKAL.DataBaseUtils.FrameFull.Data

main = do
  let fs = 4096
  let param = GlitchParam
               { segmentLength = 32
               , channel = "H1:LOSC-STRAIN"
               , chunklen = truncate $ 4*fs
               , samplingFrequency = fs
             -- * whitening
               , refpsdlen = truncate $ fs
               , whtfiltordr = 1000
               , whtCoeff = []
             -- * t-f expression
               , nfrequency = truncate $ 0.2*fs
               , ntimeSlide = truncate $ 0.03*fs
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
              , cdf'fftSize = truncate $ fs/10
              , cdf'chunkSize = truncate $ 4*fs
              }


  runGlitchMonFile param (channel param) "/home/kazu/work/data/gw150914/H-H1_LOSC_4_V1-1126259446-32.gwf"



