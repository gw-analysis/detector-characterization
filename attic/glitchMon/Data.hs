
module GlitchMon.Data
( TrigParam (..))
where

import Data.Int (Int32)

data TrigParam = TrigParam { detector :: Maybe String
                           , event_gpsstarts :: Maybe Int32
                           , event_gpsstartn :: Maybe Int32
                           , event_gpsstops :: Maybe Int32
                           , event_gpsstopn :: Maybe Int32
                           , event_cgpss :: Maybe Int32
                           , event_cgpsn :: Maybe Int32
                           , duration :: Maybe Double
                           , energy :: Maybe Double
                           , central_frequency :: Maybe Double
                           , snr :: Maybe Double
                           , significance :: Maybe Double
                           , latitude :: Maybe Double
                           , longitude :: Maybe Double
                           , chname :: Maybe String
                           , sampling_rate :: Maybe Int32
                           , segment_gpsstarts :: Maybe Int32
                           , segment_gpsstartn :: Maybe Int32
                           , segment_gpsstops :: Maybe Int32
                           , segment_gpsstopn :: Maybe Int32
                           , dq_flag :: Maybe Int32
                           , pipeline :: Maybe String
                           }

