{-******************************************
  *     File Name: StudentRayleighThreshold.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/10/01 14:37:22
  *******************************************-}

module HasKAL.StudentRayleighThreshold (
   studentThreshold
) where

import HasKAL.ExternalUtils.GSL.RandomNumberDistributions
import HasKAL.MonitorUtils.SRMon.StudentRayleighFunctions

{--  External Functions  --}
-- param1: gaussianでの閾値
-- param2: 非ガウスさ nu
-- return: student-tでの閾値
studentThreshold :: Double -> Double -> Double
studentThreshold threshold nu = studentThreshold' (gaussianProb threshold) nu

{-- Internal Functions  --}
-- +/- threshold sigma に入る確率probを返す
gaussianProb :: Double -> Double
gaussianProb threshold = (gslCdfGaussianP threshold 1.0) - (gslCdfGaussianQ threshold 1.0)

studentThreshold' :: Double -> Double -> Double
studentThreshold' prob nu = gslCdfTdistPinv (prob + probQ) nu
  where probQ = (1.0 - prob) / 2.0

