{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import qualified Language.R as R
import Language.R (R)
import Language.R.QQ
import qualified H.Prelude as H

-- Call R's FFT
r_fft :: [Double] -> R s [Double]
r_fft nums = do
    H.dynSEXP <$> [r| fft(nums_hs) |]

r_mic :: [Double] -> [Double] -> R s [Double]
r_mic x y = do
    H.dynSEXP <$> [r| require("minerva")
                      a <- mine(x_hs,y_hs)
                      c(a$MIC, a$MAS, a$MEV, a$MCN, a$`MIC-R2`,a$GMIC)
                      |]


main :: IO ()
main = do
  let a = [1,2,1] :: [Double]
      b = [2,4,3] :: [Double]
  R.withEmbeddedR R.defaultConfig $ do
    result <- R.runRegion $ r_mic a b
    print result
