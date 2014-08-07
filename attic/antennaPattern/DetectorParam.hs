module DetectorParam
( DetectorParam (name, detr, deta, detb)
, ligoHanford
)
where

import HasKAL.DetectorUtils.Detector


data DetectorParam =
  DetectorParam { name :: Detector
                , detr :: (Double, Double, Double)
                , deta :: (Double, Double, Double)
                , detb :: (Double, Double, Double)
                } deriving (Show)

ligoHanford :: DetectorParam
ligoHanford = DetectorParam { name = LIGO_Hanford
                            , detr = (-2.161414928E6, -3.834695183E6, 4.600350224E6)
                            , deta = (-0.223891216, 0.799830697, 0.556905359)
                            , detb = (-0.913978490,   0.026095321,  -0.404922650)
                            }

