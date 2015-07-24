


module HasKAL.MonitorUtils.SRMon.Signature (
   FitMethod(LSM, MLE, QUANT)
) where

data FitMethod = LSM -- ^ Least square method
               | MLE -- ^ maximum liklihood estimator
               | QUANT Double -- ^ quantile pvalue
               deriving (Show, Eq)
