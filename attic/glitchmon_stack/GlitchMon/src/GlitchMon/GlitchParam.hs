

module GlitchMon.GlitchParam
where

--import qualified Data.Vector.Storable as V
import HasKAL.DataBaseUtils.FrameFull.Data
import HasKAL.SpectrumUtils.Signature (Spectrum)
import HasKAL.TimeUtils.Signature (GPSTIME)
import HasKAL.WaveUtils.Data(WaveData)


data GlitchParam = GlitchParam
  { segmentLength :: Int
  , channel :: String
  , samplingFrequency :: Double
-- * whitening
  , traindatlen :: Double -- [s]
  , whnFrequencyResolution    :: Double -- [s]
  , whtCoeff :: [([Double],  Double)]
  , whnMethod :: WhnMethod
-- * t-f expression
  , nfrequency :: Double -- 0.2*fs
  , ntimeSlide :: Double --0.03*fs
-- * clustering
  , cutoffFractionTFT :: Double
  , cutoffFractionTFF :: Double
  , cutoffFreq :: Double
  , clusterThres :: Double
  , celement :: (Int, Int) -> [(Int, Int)]
  , minimumClusterNum ::Int
  , nNeighbor :: Int -- least number of neighbors of a pixcel
-- * clean data finder
  , cdfInterval :: Int -- ^ interval[s] to run clean data finder (default 600[s])
  , cdfparameter :: CDFParam
  , cgps  :: Maybe GPSTIME
-- * temporary data
  , refpsd :: Spectrum
  , refwave :: WaveData
  , reftime :: Double
-- * for debug
  , debugmode :: [FlagDebug]
  , debugDir :: String
  }


data WhnMethod = TimeDomain | FrequencyDomain deriving (Show)


data FlagDebug = DS | WH | TF | ETG | CL | PE | REG deriving (Show, Eq)


updateGlitchParam'segmentLength :: GlitchParam -> Int -> GlitchParam
updateGlitchParam'segmentLength x a = x {segmentLength = a}

updateGlitchParam'channel :: GlitchParam -> String -> GlitchParam
updateGlitchParam'channel x n = x {channel = n}

updateGlitchParam'traindatlen :: GlitchParam -> Double -> GlitchParam
updateGlitchParam'traindatlen x n = x {traindatlen = n}

updateGlitchParam'samplingFrequency :: GlitchParam -> Double -> GlitchParam
updateGlitchParam'samplingFrequency x fs = x {samplingFrequency = fs}

updateGlitchParam'whnFrequencyResolution :: GlitchParam -> Double -> GlitchParam
updateGlitchParam'whnFrequencyResolution x n = x {whnFrequencyResolution = n}

updateGlitchParam'whtCoeff :: GlitchParam -> [([Double],  Double)] -> GlitchParam
updateGlitchParam'whtCoeff x p = x {whtCoeff = p}

updateGlitchParam'whnMethod :: GlitchParam -> WhnMethod -> GlitchParam
updateGlitchParam'whnMethod x method = x {whnMethod = method}

updateGlitchParam'nfrequency :: GlitchParam -> Double -> GlitchParam
updateGlitchParam'nfrequency x n = x {nfrequency = n}

updateGlitchParam'ntimeSlide :: GlitchParam -> Double -> GlitchParam
updateGlitchParam'ntimeSlide x n = x {ntimeSlide = n}

updateGlitchParam'cutoffFractionTFT:: GlitchParam -> Double -> GlitchParam
updateGlitchParam'cutoffFractionTFT x n = x {cutoffFractionTFT = n}

updateGlitchParam'cutoffFractionTFF :: GlitchParam -> Double -> GlitchParam
updateGlitchParam'cutoffFractionTFF x n = x {cutoffFractionTFF = n}

updateGlitchParam'cutoffFreq :: GlitchParam -> Double -> GlitchParam
updateGlitchParam'cutoffFreq x f = x {cutoffFreq = f}

updateGlitchParam'clusterThres :: GlitchParam -> Double -> GlitchParam
updateGlitchParam'clusterThres x thres = x {clusterThres = thres}

updateGlitchParam'cdfInterval :: GlitchParam -> Int -> GlitchParam
updateGlitchParam'cdfInterval x param = x {cdfInterval = param}

updateGlitchParam'cdfparameter :: GlitchParam -> CDFParam -> GlitchParam
updateGlitchParam'cdfparameter x param = x {cdfparameter = param}

updateGlitchParam'cgps :: GlitchParam -> Maybe GPSTIME -> GlitchParam
updateGlitchParam'cgps x param = x {cgps =  param}

updateGlitchParam'refpsd :: GlitchParam -> Spectrum -> GlitchParam
updateGlitchParam'refpsd x psd = x {refpsd = psd}

updateGlitchParam'refwave :: GlitchParam -> WaveData -> GlitchParam
updateGlitchParam'refwave x w = x {refwave = w}

updateGlitchParam'reftime :: GlitchParam -> Double -> GlitchParam
updateGlitchParam'reftime x t = x {reftime = t}

updateGlitchParam'nNeighbor :: GlitchParam -> Int -> GlitchParam
updateGlitchParam'nNeighbor x param = x {nNeighbor = param}

updateGlitchParam'debugDir :: GlitchParam -> String -> GlitchParam
updateGlitchParam'debugDir x param = x {debugDir = param}


