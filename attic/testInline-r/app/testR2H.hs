{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Data.Complex
import qualified Language.R as R
import Language.R (R)
import Language.R.QQ
import qualified H.Prelude as H

-- Call R's FFT
r_fft :: [Double] -> R s [Double]
r_fft nums = do
    H.dynSEXP <$> [r| fft(nums_hs) |]

main :: IO ()
main = R.withEmbeddedR R.defaultConfig $ do
    result <- R.runRegion $ r_fft [1,2,1]
    print result
