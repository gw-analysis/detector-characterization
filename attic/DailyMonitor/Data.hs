

module Data 
where

data Monitor = TimeSeries | Spectrum | Spectrogram
             | RMSMon     | SensMon  | LTF
             | RMon       | SRMon    | LTA
  deriving ( Eq, Ord, Show, Read)

