

module GlitchParam
where

data GlitchParam = GlitchParam
  { whtfiltordr  :: Int
  , chunklen       :: Int
  , samplingFrequency :: Double
  , nfrequency :: Int
  , ntimeSlide :: Int
  , resolvTime :: Int
  , resolvFreq :: Int
  , cutoffFreq :: Double
  , clusterThres :: Double
  }
