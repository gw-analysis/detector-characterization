


import GlitchMon.GlitchMonFile
import GlitchMon.GlitchParam
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
               , ntimeSlide = truncate $ 0.01*fs
             -- * clustering
               , resolvTime = 10
               , resolvFreq = 20
               , cutoffFreq = 10
               , clusterThres = 0.01
             -- * clean data finder
               , cdfInterval = 32
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


  runGlitchMonFile param (channel param) "/home/kazu/work/data/gw150914/H-H1_LOSC_4_V1-1126259446-32.gwf"



