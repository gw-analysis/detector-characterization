

module GlitchParam
where

--import qualified Data.Vector.Storable as V
import HasKAL.SpectrumUtils.Signature (Spectrum)
import HasKAL.WaveUtils.Data(WaveData)


data GlitchParam = GlitchParam
  { chunklen       :: Int
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

updateGlitchParam'chunklen :: GlitchParam -> Int -> GlitchParam
updateGlitchParam'chunklen x n = x {chunklen = n}

updateGlitchParam'samplingFrequency :: GlitchParam -> Double -> GlitchParam
updateGlitchParam'samplingFrequency x fs = x {samplingFrequency = fs}

updateGlitchParam'whtfiltordr :: GlitchParam -> Int -> GlitchParam
updateGlitchParam'whtfiltordr x n = x {whtfiltordr = n}

updateGlitchParam'whtCoeff :: GlitchParam -> [([Double],  Double)] -> GlitchParam
updateGlitchParam'whtCoeff x p = x {whtCoeff = p}

updateGlitchParam'nfrequency :: GlitchParam -> Int -> GlitchParam
updateGlitchParam'nfrequency x n = x {nfrequency = n}

updateGlitchParam'ntimeSlide :: GlitchParam -> Int -> GlitchParam
updateGlitchParam'ntimeSlide x n = x {ntimeSlide = n}

updateGlitchParam'resolvTime :: GlitchParam -> Int -> GlitchParam
updateGlitchParam'resolvTime x n = x {resolvTime = n}

updateGlitchParam'resolvFreq :: GlitchParam -> Int -> GlitchParam
updateGlitchParam'resolvFreq x n = x {resolvFreq = n}

updateGlitchParam'cutoffFreq :: GlitchParam -> Double -> GlitchParam
updateGlitchParam'cutoffFreq x f = x {cutoffFreq = f}

updateGlitchParam'clusterThres :: GlitchParam -> Double -> GlitchParam
updateGlitchParam'clusterThres x thres = x {clusterThres = thres}

updateGlitchParam'refpsd :: GlitchParam -> Spectrum -> GlitchParam
updateGlitchParam'refpsd x psd = x {refpsd = psd}

updateGlitchParam'refwave :: GlitchParam -> WaveData -> GlitchParam
updateGlitchParam'refwave x w = x {refwave = w}

updateGlitchParam'reftime :: GlitchParam -> Double -> GlitchParam
updateGlitchParam'reftime x t = x {reftime = t}


