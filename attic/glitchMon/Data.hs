
module Data
( TrigParam (..))
where


data TrigParam = TrigParam { detector :: Maybe String
                           , event_gpsstarts :: Maybe Int
                           , event_gpsstartn :: Maybe Int
                           , event_gpsstops :: Maybe Int
                           , event_gpsstopn :: Maybe Int
                           , duration :: Maybe Double
                           , energy :: Maybe Double
                           , central_frequency :: Maybe Double
                           , snr :: Maybe Double
                           , significance :: Maybe Double
                           , latitude :: Maybe Double
                           , longitude :: Maybe Double
                           , chname :: Maybe String
                           , sampling_rate :: Maybe Int
                           , segment_gpsstarts :: Maybe Int
                           , segment_gpsstartn :: Maybe Int
                           , segment_gpsstops :: Maybe Int
                           , segment_gpsstopn :: Maybe Int
                           , dq_flag :: Maybe Int
                           , pipeline :: Maybe String
                           }

