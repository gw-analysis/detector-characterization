-- #!/usr/bin/env stack
-- stack --resolver=lts-5.2 --extra-lib-dirs=/home/detchar/tools/lib --extra-include-dirs=/home/detchar/tools/include runghc --package=HasKAL

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Data.Maybe (fromMaybe)
import HasKAL.ExternalUtils.LIGO.NDS2.Function
import HasKAL.TimeUtils.GPSfunction (time2gps)
import qualified Data.Vector.Storable as V

main :: IO ()
main = do
  let chname = "K1:PEM-TEMPERATURE_RACK_IOO"
      fs = 16
      ch = [(chname,fs)]
      ip_nds = "10.68.10.122"
      gpss = read (time2gps "2017-02-01 00:00:00 JST") :: Int
      gpse = read (time2gps "2017-02-01 00:00:10 JST") :: Int
      port = 8088
      vs' = getData ip_nds port ch gpss gpse 1
      vs = fromMaybe (error "data not found") vs'
  print "retrieving data from the KAGRA NDS server"
  print $ take 5 $ V.toList $ head (head vs)
  print "Current number of channels daqed in KAGRA"
  print $ getNumberOfChannels ip_nds port gpss
  print "Current channel list"
  let ch0 = head $ getChannels ip_nds port gpss
  print ch0
  getDataStdout ip_nds port ch gpss gpse 5
  print "test finished."
