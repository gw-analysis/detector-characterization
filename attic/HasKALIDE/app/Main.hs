{-# LANGUAGE QuasiQuotes #-}
module Main where

import qualified Foreign.R as R
import Foreign.R (SEXP, SEXPTYPE)
import Language.R.Instance as R
import Language.R.QQ
import HasKAL.TimeUtils.GPSfunction (getCurrentGps)

hello :: String -> R s ()
hello name = do
    [r| print(s_hs) |]
    return ()
  where
    s = "CurrentGPS time is " ++ name ++ "."

main :: IO ()
main = do
  gps <- getCurrentGps 
--  let gps = "111"
  R.withEmbeddedR R.defaultConfig $ R.runRegion $ hello gps
