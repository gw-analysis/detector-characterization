{-******************************************
  *     File Name: StudentRayleighThreshold.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/10/01 18:15:47
  *******************************************-}

module HasKAL.MonitorUtils.SRMon.StudentRayleighThreshold (
   studentThreshold
) where

import HasKAL.ExternalUtils.GSL.RandomNumberDistributions
import HasKAL.MonitorUtils.SRMon.StudentRayleighFunctions

{--  Test Code  --}
-- main :: IO ()
-- main = do
--   print $ studentThreshold 1.0 10.0


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
studentThreshold' prob nu = gslCdfTdistPinv (0.5 + prob/2.0) nu

