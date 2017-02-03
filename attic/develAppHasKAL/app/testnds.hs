-- #!/usr/bin/env stack
-- stack --resolver=lts-5.2 --extra-lib-dirs=/home/detchar/tools/lib --extra-include-dirs=/home/detchar/tools/include runghc --package=HasKAL

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import HasKAL.ExternalUtils.LIGO.NDS2.Function ( ndsGetData
                                               , ndsGetChannels
                                               , ndsGetNumberOfChannels
                                               )
import HasKAL.TimeUtils.GPSfunction (time2gps)
import qualified Data.Vector.Storable as V

main :: IO ()
main = do
  let chname = "K1:PEM-TEMPERATURE_RACK_IMC"
      fs = 16
      ch = [(chname,fs)]
      ip_nds = "10.68.10.121"
      gpss = read (time2gps "2017-02-01 00:00:00 JST") :: Int
      gpse = read (time2gps "2017-02-01 00:00:10 JST") :: Int
      port = 8088
      vs = ndsGetData ip_nds port ch gpss gpse 10
  print "retrieving data from the KAGRA NDS server"
  print $ take 5 $ V.toList $ head (head vs)
