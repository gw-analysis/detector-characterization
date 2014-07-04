module Detector
(
  Detector
--, LIGO
)
where

data Detector = GEO | LIGO LIGOLocation | KAGRA | VIRGO | INDIGO
  deriving (Show)

data  LIGOLocation = Livingston | Hanford
  deriving (Show)


