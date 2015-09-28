
import qualified Data.Vector.Generic as DVG
import qualified Numeric.LinearAlgebra as NLA
import Data.Maybe (fromMaybe, fromJust)
import Numeric (showFFloat)

import HasKAL.PlotUtils.HROOT.PlotGraph
import HasKAL.FrameUtils.Function (readFrameV)
import HasKAL.SpectrumUtils.SpectrumUtils (gwpsdV)
import HasKAL.SpectrumUtils.Signature
import HasKAL.MonitorUtils.RMSMon.RMSMon (rmsDailyMon)
import HasKAL.TimeUtils.GPSfunction

import HasKAL.DataBaseUtils.Function (kagraDataGet)

{-- memo
    running time : ~10min 15s = 615s
--}


main = do
 let fs = 2048::Double
     totalduration  = (24*60*60) :: Int -- 1day = 86400s

     {-- parameters --}
 let gps = 1120544000::Int
 let jst = gps2localTime (toInteger gps) "JST" ::String
 let xlabel = "hour[h] since "  ++  show jst ::String
     ylabel = "Voltage[V]"::String

 {-- open frame file --}
-- maybexs <- mapM (readFrameV channel) filelist
-- let xs = map (fromMaybe (error " no data in the file.")) maybexs
-- let ys = DVG.concat xs



 let channel  = "K1:PEM-EX_MAG_X_FLOOR"
 ysmaybe <- kagraDataGet gps totalduration channel
 case ysmaybe of
     Nothing -> print "Can't find channel"
     _       -> do
       let ys = fromJust ysmaybe
       let fname = "hoge.png"
       let color = [BLUE, RED, PINK]
       let freq  = [(0.1, 1.0), (1.0, 4.0), (4.0, 8.0)]::[(Double, Double)]
       let rms   = rmsDailyMon fs ys freq
       let rms_max = DVG.maximum $ DVG.concat ( map snd rms)
       oPlotV Linear LinePoint 1 color (xlabel, ylabel) 0.05 "RMSMon (BLUE:0.1-1Hz, RED:1-4Hz, PINK:4-8Hz)" fname ((0,0),(0,rms_max*1.2)) rms
 
 return 0


