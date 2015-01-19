{- |
Module      : HasKAL.MonitorUtils.SRMon.Signature
Description : This is documentation tests.
Copyright   : (c) WhoAmI, 2014
License     : ???
Maintainer  : hoge@hoge.com
Stability   : test
Portability : POSIX

fitting method of Student-Rayleigh Monitor
-}

module HasKAL.MonitorUtils.SRMon.Signature (
   FitMethod(LSM, MLE, QUANT)
) where

data FitMethod = LSM -- ^ Least square method
               | MLE -- ^ maximum liklihood estimator
               | QUANT Double -- ^ quantile pvalue
               deriving (Show, Eq)
