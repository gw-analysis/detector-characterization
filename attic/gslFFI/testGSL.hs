#!/usr/bin/env runghc
{-******************************************
  *     File Name: testGSL.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/05/27 19:40:38
  *******************************************-}

import qualified WrapGSL.RandomNumberDistributions as WGRND
import qualified WrapGSL.SpecialFunctions as WGSF
main :: IO ()
main = do
  let pVal = [0.5, 0.7, 0.8, 0.9] :: [Double]
  let sigma = 1.0 :: Double
  print $ map (flip WGRND.gslCdfRayleighPinv sigma) pVal

  let x = 1.214214
  print $ WGSF.gslSfGamma x
