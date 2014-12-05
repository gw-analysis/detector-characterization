{-******************************************
  *     File Name: Signature.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/12/05 15:31:20
  *******************************************-}

module HasKAL.MonitorUtils.SRMon.Signature (
   FitMethod(LSM, MLE, QUANT)
) where

data FitMethod = LSM | MLE | QUANT Double deriving (Show, Eq)
