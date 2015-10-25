

module GlitchParam
where

--import qualified Data.Vector.Storable as V
import HasKAL.SpectrumUtils.Signature (Spectrum)
import HasKAL.WaveUtils.Data(WaveData)


data GlitchParam = GlitchParam
  {
    chunklen       :: Int
  , samplingFrequency :: Double
-- * whitening
  , whtfiltordr  :: Int
  , whtCoeff :: [([Double],  Double)]
-- * t-f expression
  , nfrequency :: Int
  , ntimeSlide :: Int
-- * clustering
  , resolvTime :: Int
  , resolvFreq :: Int
  , cutoffFreq :: Double
  , clusterThres :: Double
-- * temporary data
  , refpsd :: Spectrum
  , refwave :: WaveData
  , reftime :: Double
  }

updateParam = undefined


