
module HasKAL.MonitorUtils.CorrelationMon.CorrelationMethod(
       CorrelationMethod(Pearson, MIC)
       ) where

data CorrelationMethod = Pearson | MIC deriving (Eq, Read)
