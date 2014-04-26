#!/usr/bin/env runghc
{-******************************************
  *     File Name: testGSL.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/04/12 19:33:49
  *******************************************-}

import qualified WrapGSL.RandomNumberDistributions as WGRND

main :: IO ()
main = do
  let pVal = [0.5, 0.7, 0.8, 0.9] :: [Double]
  let sigma = 1.0 :: Double
  print $ map (flip WGRND.gslCdfRayleighPinv sigma) pVal

