
module HasKAL.MonitorUtils.CorrelationMon.CorrelationMethod(
       CorrelationMethod(Pearson, MIC, DistCor)
       ) where

data CorrelationMethod = Pearson | MIC | DistCor deriving (Eq, Read, Show)
