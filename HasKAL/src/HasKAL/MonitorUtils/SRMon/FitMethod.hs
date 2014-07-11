{-******************************************
  *     File Name: FitMethod.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/07/11 17:54:50
  *******************************************-}

module HasKAL.MonitorUtils.SRMon.FitMethod (
   FitMethod(LSM, MLE, QUANT)
) where

data FitMethod = LSM | MLE | QUANT Double deriving (Show, Eq)
