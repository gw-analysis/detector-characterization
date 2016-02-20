


import GlitchMon.GlitchMonFile
import GlitchMon.GlitchParam
import HasKAL.DataBaseUtils.FrameFull.Data

main = do
  let fs = 4096
  let param = GlitchParam
               { channel = "H1:LOSC-STRAIN"
               , chunklen = truncate $ 4*fs
               , samplingFrequency = fs
             -- * whitening
               , refpsdlen = truncate $ fs/2
               , whtfiltordr = 1000
               , whtCoeff = []
             -- * t-f expression
               , nfrequency = 1000
               , ntimeSlide = 100
             -- * clustering
               , resolvTime = 10
               , resolvFreq = 10
               , cutoffFreq = 10
               , clusterThres = 3
             -- * clean data finder
               , cdfInterval = 600
               , cdfparameter = cdfp
               , cgps = Nothing
             -- * temporary data
--               , refpsd =
--               , refwave =
               , reftime = 0
               }

      cdfp = CDFParam
              { cdf'samplingFrequency = fs
              , cdf'cutoffFrequencyLow = 10
              , cdf'cutoffFrequencyHigh = 500
              , cdf'blockSize = 50
              , cdf'fftSize = truncate $ fs/10
              , cdf'chunkSize = truncate $ 4*fs
              }


  runGlitchMonFile param (channel param) "./gw150914/H-H1_LOSC_4_V1-1126259446-32.gwf"



