import qualified Data.Vector.Storable as V
import qualified Data.Vector.Generic as DVG
--import qualified Numeric.LinearAlgebra as NLA
import Data.Maybe (fromMaybe, fromJust)
import Numeric (showFFloat)
import System.Environment (getArgs)
import System.IO (stdout, hPutStrLn)

import HasKAL.TimeUtils.Function (deformatGPS)
import HasKAL.TimeUtils.GPSfunction (time2gps, gps2localTime)
import HasKAL.PlotUtils.HROOT.PlotGraph
import HasKAL.FrameUtils.Function (readFrameV)
import HasKAL.SpectrumUtils.Signature
import HasKAL.MonitorUtils.RMSMon.RMSMon (rmsMon)

import HasKAL.DataBaseUtils.FrameFull.Function (kagraDataGet, kagraDataFind, kagraDataGetC, kagraWaveDataGetC)
import HasKAL.FrameUtils.FrameUtils (getSamplingFrequency, getUnitY)

import HasKAL.WaveUtils.Data (WaveData(..))

{-- memo
    running time (24hour data) : ~2m30s
--}

main = do
 args <- getArgs
 (year, month, day, channel, f1low, f1high, f2low, f2high, f3low, f3high) <- case length args of
     10 -> return (args!!0, show0 2 (args!!1), show0 2 (args!!2), args!!3, args!!4, args!!5, args!!6, args!!7, args!!8, args!!9)
     8 ->  return (args!!0, show0 2 (args!!1), show0 2 (args!!2), args!!3, args!!4, args!!5, args!!6, args!!7, "0", "0")
     6 ->  return (args!!0, show0 2 (args!!1), show0 2 (args!!2), args!!3, args!!4, args!!5, "0", "0", "0", "0")
     4 ->  return (args!!0, show0 2 (args!!1), show0 2 (args!!2), args!!3, "0.1", "10", "50", "200", "300", "1000")
     _ ->  error "Usage: RMSMon yyyy mm dd channel (f1low f1high f2low f2high f3low f3high)\n(frequency bands are option)\nexample)\n./RMSMon 2015 12 15 K1:PSL-PMC_FAST_MON_OUT_DQ 0.1 10 50 200 300 1000"


 {-- parameters --}
 let gps = read $ time2gps $ year++"-"++month++"-"++day++" 00:00:00 JST"
-- let totalduration = 86400 :: Int -- 1day = 86400s
 let totalduration = 2700 :: Int -- 1day = 86400s

 let jst = gps2localTime (toInteger gps) "JST" ::String
 let xlabel = "hour[h] since "  ++  show jst ::String

 filesmaybe <- kagraDataFind (fromIntegral gps) (fromIntegral totalduration) channel
 let file = case filesmaybe of
              Nothing -> error $ "Can't find file: "++year++"/"++month++"/"++day
              _ -> head $ fromJust filesmaybe

 fsmaybe <- getSamplingFrequency file channel
 ysmaybe <- kagraDataGet gps totalduration channel
 let (ys, fs) = case (ysmaybe, fsmaybe) of
                 (Just a, Just b) -> (a, b)
                 (Nothing, _) -> error $ "Can't read data: "++ channel ++"-"++year++"/"++month++"/"++day
                 (_, Nothing) -> error $ "Can't read sampling frequency: "++ channel ++"-"++year++"/"++month++"/"++day

 -- ここでtys -> ysへ変換する
 let duration = 100 :: Int
 kagraWaveDataGetC gps duration channel >>= \judge -> case judge of
    Nothing  -> error $ "Can't find file: "++year++"/"++month++"/"++day
    Just wav -> do let td = concatMap timendat wav
                   mapM_ (\(t,x) -> hPutStrLn stdout $ (show t)++" "++show x) td

-- tysmaybe <- kagraDataGetC gps totalduration channel
-- let (tys, fs) = case (tysmaybe2, fsmaybe) of
--                 (Just a, Just b) -> (a, b)
--                 (Nothing, _) -> error $ "Can't read data: "++ channel ++"-"++year++"/"++month++"/"++day
--                 (_, Nothing) -> error $ "Can't read sampling frequency: "++ channel ++"-"++year++"/"++month++"/"++day

 mbUnit <- getUnitY file channel
 let unit = case mbUnit of
             Just x  -> "["++x++"]"
             Nothing -> "[]"

 let f1band = ((read f1low::Double), (read f1high::Double))
     f2band = ((read f2low::Double), (read f2high::Double))
     f3band = ((read f3low::Double), (read f3high::Double))
     freq  = [f1band, f2band, f3band]::[(Double, Double)]
 let nmon = floor (900 * fs) ::Int -- 86400s / 96[chunk] = 900[s]
 let rms   = rmsMon nmon fs ys freq
     rms_max = DVG.maximum $ DVG.concat ( map snd rms)
     rms_min = DVG.minimum $ DVG.concat ( map snd rms)

 let color = [BLUE, GREEN, RED, PINK, CYAN]
     title = setTitle f1low f1high f2low f2high f3low f3high channel
     fname = channel ++ "-" ++ year ++ "-" ++ month ++ "-" ++ day ++ "_RMSMon.png"

-- oPlotDateV LogY LinePoint 1 color (xlabel, ylabel) 0.05 title fname ((0,0),(rms_min*0.8,rms_max*1.2)) gps rms
 oPlotDateV LogY LinePoint 1 color (xlabel, "RMS" ++unit) 0.05 title fname ((0,0),(rms_min*0.8,rms_max*1.2)) gps rms
 return 0


{-- Internal Functions --}
show0 :: Int -> String -> String
show0 digit number
  | len < digit = (concat $ replicate (digit-len) "0") ++ number
  | otherwise   = number
  where len = length number

setTitle :: String -> String -> String -> String -> String -> String -> String -> String
setTitle f1low f1high f2low f2high f3low f3high channel = 
         "RMS Mon(BLUE=" ++ f1low ++ "-" ++ f1high ++ "Hz, GREEN=" ++ f2low ++ "-" ++ f2high ++ "Hz, RED=" ++ 
             f3low ++ "-" ++ f3high ++ " : " ++ channel

timendat y = let t = deformatGPS $ startGPSTime y
                 fs = samplingFrequency y
                 tl = [t+i/fs|i<-[0.0,1.0..fromIntegral (length xl) -1.0]] 
                 xl = V.toList $ gwdata y
              in zip tl xl

--zeroPaddingPieceData :: [(GPSTIME, V.Vector Double)] -- ^ time series data missing
--		     -> Double -- ^ sampling rate [Hz]
--		     -> V.Vector Double -- ^ coutinuous time series data
--zeroPaddingPieceData tys fs
--  | (length tys) == 1 = snd (tys!!0)
--  | otherwise = do
--      let ys = map (\(gpss, yss) -> ) tys



