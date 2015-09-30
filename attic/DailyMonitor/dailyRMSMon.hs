
import qualified Data.Vector.Generic as DVG
import qualified Numeric.LinearAlgebra as NLA
import Data.Maybe (fromMaybe, fromJust)
import Numeric (showFFloat)
import System.Environment (getArgs)

import HasKAL.TimeUtils.GPSfunction (time2gps, gps2localTime)
import HasKAL.PlotUtils.HROOT.PlotGraph
import HasKAL.FrameUtils.Function (readFrameV)
import HasKAL.SpectrumUtils.SpectrumUtils (gwpsdV)
import HasKAL.SpectrumUtils.Signature
import HasKAL.MonitorUtils.RMSMon.RMSMon (rmsDailyMon)

import HasKAL.DataBaseUtils.Function (kagraDataGet)

{-- memo
    running time : ~10min 15s = 615s

    usage : 

--}

main = do
 args <- getArgs
 (channel, year, month, day, f1low, f1high, f2low, f2high, f3low, f3high) <- case length args of
     10 -> return (args!!0, args!!1, show0 2 (args!!2), show0 2 (args!!3), args!!4, args!!5, args!!6, args!!7, args!!8, args!!9)
     8 -> return (args!!0, args!!1, show0 2 (args!!2), show0 2 (args!!3), args!!4, args!!5, args!!6, args!!7, "0", "0")
     6 -> return (args!!0, args!!1, show0 2 (args!!2), show0 2 (args!!3), args!!4, args!!5, "0", "0", "0", "0")
     4 -> return (args!!0, args!!1, show0 2 (args!!2), show0 2 (args!!3), "0", "0", "0", "0", "0", "0")
     _ -> error "Usage: dailyRMSMon channel yyyy mm dd f1low f1high f2low f2high f3low f3high\nex)\ndailyRMSMon K1:PEM-EX_MAG_X_FLOOR 2015 7 15 0.1 1 1 4 4 8"


     {-- parameters --}
 let gps = read $ time2gps $ year++"-"++month++"-"++day++" 00:00:00 JST"
-- let gps = 1120544000::Int
 let fs = 2048::Double
--     totalduration  = 86400 :: Int -- 1day = 86400s
     totalduration = 600 :: Int
 let jst = gps2localTime (toInteger gps) "JST" ::String
 let xlabel = "hour[h] since "  ++  show jst ::String
     ylabel = "Voltage[V]"::String


 ysmaybe <- kagraDataGet gps totalduration channel
 case ysmaybe of
     Nothing -> error $ "Can't find file: "++year++"/"++month++"/"++day
     _       -> do
       let ys = fromJust ysmaybe
       let fname = channel ++ "-" ++ year ++ "-" ++ month ++ "-" ++ day ++ "_RMS_Mon.png"
       let color = [BLUE, RED, PINK]
       let f1band = ((read f1low::Double), (read f1high::Double))
           f2band = ((read f2low::Double), (read f2high::Double))
           f3band = ((read f3low::Double), (read f3high::Double))
       let freq  = [f1band, f2band, f3band]::[(Double, Double)]
       let rms   = rmsDailyMon fs ys freq
       let rms_max = DVG.maximum $ DVG.concat ( map snd rms)

       let title = setTitle f1low f1high f2low f2high f3low f3high channel
       oPlotV Linear LinePoint 1 color (xlabel, ylabel) 0.05 title fname ((0,0),(0,rms_max*1.2)) rms
 return 0

{-- Internal Functions --}
show0 :: Int -> String -> String
show0 digit number
  | len < digit = (concat $ replicate (digit-len) "0") ++ number
  | otherwise   = number
  where len = length number

setTitle :: String -> String -> String -> String -> String -> String -> String -> String
setTitle f1low f1high f2low f2high f3low f3high channel = 
         "RMS Mon(BLUE=" ++ f1low ++ "-" ++ f1high ++ "Hz, RED=" ++ f2low ++ "-" ++ f2high ++ "Hz, PINK=" ++ 
             f3low ++ "-" ++ f3high ++ " : " ++ channel
