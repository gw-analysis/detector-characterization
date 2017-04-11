
module GlitchMon.Data
( TrigParam (..)
 , PlotOPT (..))
where

import Data.Int (Int32)

data TrigParam = TrigParam { glitch_ID :: Maybe Int
                           , detector :: Maybe String
                           , event_gpsstarts :: Maybe Int32
                           , event_gpsstartn :: Maybe Int32
                           , event_gpsstops :: Maybe Int32
                           , event_gpsstopn :: Maybe Int32
                           , event_fmin :: Maybe Double
                           , event_fmax :: Maybe Double
                           , event_cgpss :: Maybe Int32
                           , event_cgpsn :: Maybe Int32
                           , duration :: Maybe Double
                           , energy :: Maybe Double
                           , island_size :: Maybe Int32
                           , central_frequency :: Maybe Double
                           , snr :: Maybe Double
                           , significance :: Maybe Double
                           , latitude :: Maybe Double
                           , longitude :: Maybe Double
                           , channel :: Maybe String
                           , sampling_rate :: Maybe Int32
                           , segment_gpsstarts :: Maybe Int32
                           , segment_gpsstartn :: Maybe Int32
                           , segment_gpsstops :: Maybe Int32
                           , segment_gpsstopn :: Maybe Int32
                           , dq_flag :: Maybe Int32
                           , pipeline :: Maybe String
                           , injection :: Maybe Int32
                           , hrss :: Maybe Double
                           }

data PlotOPT = GPS | CentralFrequency | SNR | DQFlag | Significance | Size | Energy | Duration | CentralGPS deriving (Show,Read,Eq)
