module Detector
(
  Detector
, LIGO
)
where

data Detector = GEO | LIGO | KAGRA | VIRGO | INDIGO
  deriving (Show)

data LIGO = Livingston | Hanford
  deriving (Show)


