module HasKAL.SignalProcessingUtils.WindowType
where
  data WindowType = Hann | Blackman | Hamming | TaperedCosine | Kaiser
    deriving (Eq)
