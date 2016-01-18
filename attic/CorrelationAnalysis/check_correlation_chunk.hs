import qualified Data.Vector.Generic as DVG
import qualified Numeric.LinearAlgebra as NLA
import Data.Maybe (fromMaybe, fromJust)
import qualified Data.Vector.Storable as S
import System.Environment (getArgs)

import HasKAL.TimeUtils.GPSfunction (time2gps, gps2localTime)
import HasKAL.DataBaseUtils.XEndEnv.Function (kagraDataGet, kagraDataFind)
import HasKAL.FrameUtils.FrameUtils (getSamplingFrequency)
import HasKAL.MonitorUtils.CorrelationMon.CalCorrelation
import HasKAL.MonitorUtils.CorrelationMon.CorrelationMethod
import HasKAL.PlotUtils.HROOT.PlotGraph

main = do
 args <- getArgs
 (year, month, day, ch1, ch2) <- case length args of
    5 ->  return (args!!0, show0 2 (args!!1), show0 2 (args!!2), args!!3, args!!4)
    _ ->  error "Usage: check_correlation_chunk yyyy mm dd ch1 ch2\n\ncheck_correlation_chunk 2015 7 15 K1:PEM-EX_MAG_X_FLOOR K1:PEM-EX_MAG_Y_FLOOR"


 {-- parameters --}
 let gps = read $ time2gps $ year++"-"++month++"-"++day++" 00:00:00 JST"
 let totalduration = 1000 :: Int
     chunkduration = 10 :: Double 

 let jst = gps2localTime (toInteger gps) "JST" ::String

 filesmaybe <- kagraDataFind (fromIntegral gps) (fromIntegral totalduration) ch1
 let file = case filesmaybe of
              Nothing -> error $ "Can't find file: "++year++"/"++month++"/"++day
              _ -> head $ fromJust filesmaybe

 fsmaybe1 <- getSamplingFrequency file ch1
 ysmaybe1 <- kagraDataGet gps totalduration ch1
 let (ys1, fs1) = case (ysmaybe1, fsmaybe1) of
                 (Just a, Just b) -> (a, b)
                 (Nothing, _) -> error $ "Can't read data: "++ ch1 ++"-"++year++"/"++month++"/"++day
                 (_, Nothing) -> error $ "Can't read sampling frequency: "++ ch1 ++"-"++year++"/"++month++"/"++day

 fsmaybe2 <- getSamplingFrequency file ch2
 ysmaybe2 <- kagraDataGet gps totalduration ch2
 let (ys2, fs2) = case (ysmaybe2, fsmaybe2) of
                 (Just a, Just b) -> (a, b)
                 (Nothing, _) -> error $ "Can't read data: "++ ch2 ++"-"++year++"/"++month++"/"++day
                 (_, Nothing) -> error $ "Can't read sampling frequency: "++ ch2 ++"-"++year++"/"++month++"/"++day
 
 let (time, rho, tshift) = correlationChunkV Pearson ys1 ys2 (fromIntegral totalduration) chunkduration fs1 10
 let xlabel = "hour[h] since "  ++  show jst :: String
     ylabel = "Pearson rho" :: String
     ylabel2 = "rho's time shift" :: String
     title = ch1 ++ " - " ++ ch2 :: String
 plotV Linear LinePoint 1 RED (xlabel, ylabel) 0.05 title "correlation_time-rho.png" ((0, 0),(0, 0)) (time, rho)
 plotV Linear LinePoint 1 RED (xlabel, ylabel2) 0.05 title "correlation_time-tshift.png" ((0, 0),(0, 0)) (time, tshift)


 let (timel, rhol, tshiftl) = correlationChunk Pearson (S.toList ys1) (S.toList ys2) (fromIntegral totalduration) chunkduration fs1 10
 plot Linear LinePoint 1 RED (xlabel, ylabel) 0.05 title "correlation_time-rho_list.png" ((0, 0),(0, 0)) $ zip timel rhol
 plot Linear LinePoint 1 RED (xlabel, ylabel2) 0.05 title "correlation_time-tshift_list.png" ((0, 0),(0, 0)) $ zip timel tshiftl

-- print rho


{-- Internal Functions --}
show0 :: Int -> String -> String
show0 digit number
  | len < digit = (concat $ replicate (digit-len) "0") ++ number
  | otherwise   = number
  where len = length number

