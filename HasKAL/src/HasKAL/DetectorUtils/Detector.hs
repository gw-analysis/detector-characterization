module HasKAL.DetectorUtils.Detector
( Detector
, LIGOLocation
)
where

data Detector = DECIGO | ELISA | ET | GEO | INDIGO | KAGRA | LIGO LIGOLocation| VIRGO
  deriving (Show, Eq)

data LIGOLocation = Livingston | Hanford
  deriving (Show, Eq)





