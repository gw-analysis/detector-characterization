import qualified Data.Vector.Storable as V
import qualified Data.Vector.Generic as DVG
import Data.Maybe (fromMaybe, fromJust)
import System.Environment (getArgs)
import System.IO (stdout, hPutStrLn)

import HasKAL.TimeUtils.Function (deformatGPS)
import HasKAL.TimeUtils.GPSfunction (time2gps, gps2localTime)
import HasKAL.PlotUtils.HROOT.PlotGraph
import HasKAL.FrameUtils.Function (readFrameV)
import HasKAL.SpectrumUtils.Signature
import HasKAL.MonitorUtils.RMSMon.RMSMon (rmsMon)

import HasKAL.DataBaseUtils.FrameFull.Function (kagraDataGet, kagraDataFind, kagraWaveDataGet0)
import HasKAL.WaveUtils.Data (WaveData(..))
import HasKAL.FrameUtils.FrameUtils (getSamplingFrequency, getUnitY)

import HasKAL.WaveUtils.Data (WaveData(..))

main = do
 args <- getArgs
 gps' <- case length args of
     1 -> return (args!!0)
     _ -> error "Usage: ./readTest 1134120000"

-- let gps = 1134120000
 let gps = read gps' :: Int
 let totalduration = 5000 :: Int

 let channel = "K1:PSL-PMC_FAST_MON_OUT_DQ"

 filesmaybe <- kagraDataFind (fromIntegral gps) (fromIntegral totalduration) channel
 let file = case filesmaybe of
              Nothing -> error $ "Can't find file" ++ (show gps)
              _ -> head $ fromJust filesmaybe

 fsmaybe <- getSamplingFrequency file channel
 ysmaybe <- kagraWaveDataGet0 gps totalduration channel
 let (ys, fs) = case (ysmaybe, fsmaybe) of
                 (Just a, Just b) -> (a, b)
                 (Nothing, _) -> error $ "Can't read data: "++ (show gps)
                 (_, Nothing) -> error $ "Can't read sampling frequency: "++ channel

 let lengthgwdata = realToFrac $ V.length $ gwdata ys

 hPutStrLn stdout $ "gps    : " ++ (show gps)
 hPutStrLn stdout $ "fs     : " ++ (show fs)
-- hPutStrLn stdout $ "length : " ++ (show lengthgwdata)

 let tv = V.fromList [1.0/fs, 2.0/fs..lengthgwdata/fs]
 let xv = gwdata ys
 let filename = (show gps) ++ ".png"
 let title = "GPS : " ++ (show gps)
 plotV Linear Line 1 BLUE ("gps[s]", "") 0.05 title filename ((0,0),(0,0)) (tv, xv)



 return 0