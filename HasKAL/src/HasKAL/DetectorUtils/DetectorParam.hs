module HasKAL.DetectorUtils.DetectorParam
( DetectorParam (name, detr, deta, detb)
, ligoHanford
, ligoLivingston
, kagra
, virgo
, geo600
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
                            , detb = (-0.913978490, 0.026095321, -0.404922650)
                            }

ligoLivingston :: DetectorParam
ligoLivingston = DetectorParam { name = LIGO_Livingston
                               , detr = (-7.427604192E4,  -5.496283721E6,  3.224257016E6)
                               , deta = (-0.954574615,  -0.141579994,  -0.262187738)
                               , detb = (0.297740169,  -0.487910627,  -0.820544948)
                               }

virgo :: DetectorParam
virgo = DetectorParam { name = VIRGO
                      , detr = (4.5463741e6,  8.429897e5,  4.378577e6)
                      , deta = (-0.700458309,   0.208487795,  0.682562083)
                      , detb = (-0.053791331,  -0.969082169,  0.240803326)
                      }

kagra :: DetectorParam
kagra = DetectorParam { name = KAGRA
                      , detr = (-3776899.062,   3483900.163,  3766657.585)
                      , deta = (-0.4300,    -0.8363,   0.3400)
                      , detb = (0.6821,    -0.0542,   0.7292)
                      }

geo600 :: DetectorParam
geo600 = DetectorParam { name = GEO
                       , detr = (3.8563112e6,   6.665978e5,  5.0196406e6)
                       , deta = (-0.445184239,   0.866534205,   0.225675575)
                       , detb = (-0.626000687,  -0.552167273,   0.550667271)
                       }



