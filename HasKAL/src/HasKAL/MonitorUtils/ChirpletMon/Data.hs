
module HasKAL.MonitorUtils.ChirpletMon.Data
( ChirpletParam(..)
, ChirpletGram(..)
, ChirpletPlotParam(..)
) where

data ChirpletParam = ChirpletParam
  { alpha :: Double
  , ipath :: Int
  } deriving (Show, Eq, Read)

data ChirpletGram = ChirpletGram
  { time      :: [Double]
  , frequency :: [Double]
  , cost      :: [Double]
  } deriving (Show, Eq, Read)

data ChirpletPlotParam = ChirpletPlotParam
  { xrange :: (Double, Double)
  , yrange :: (Double, Double)
  }
