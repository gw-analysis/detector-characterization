
module HasKAL.DataBaseUtils.FrameFull.Data
where


data CDFParam = CDFParam { cdf'samplingFrequency :: Double
                         , cdf'cutoffFrequencyLow :: Double
                         , cdf'cutoffFrequencyHigh :: Double
                         , cdf'blockSize :: Int
                         , cdf'fftSize :: Int
                         , cdf'chunkSize :: Int
                         }

