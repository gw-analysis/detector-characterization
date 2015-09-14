
import qualified Data.Vector.Generic as DVG
import qualified Numeric.LinearAlgebra as NLA
import Data.Maybe (fromMaybe, fromJust)
import Numeric (showFFloat)

import HasKAL.PlotUtils.HROOT.PlotGraph
import HasKAL.FrameUtils.Function (readFrameV)
import HasKAL.SpectrumUtils.SpectrumUtils (gwpsdV)
import HasKAL.SpectrumUtils.Signature
import HasKAL.MonitorUtils.RMSMon.RMSMon (rmsMon)
import HasKAL.TimeUtils.GPSfunction

import ReadFiles

--import HasKAL.DataBaseUtils.Function (kagraDataGet)

main = do

 {-- open frame file --}
 let fs = 2048::Double
 let nfile = 10 :: Int -- you can change
     filelist = take nfile testFiles

 let totalduration  = 32 * nfile :: Int
     nSplit = 20 :: Int -- you can change

 let gps = 1124077533::Integer
 let jst = gps2localTime gps "JST" ::String
 let gpsstart = 77533::Double
 let xlabel = "time[s] since "  ++  show jst ::String
     ylabel = "Voltage[V]"::String


 -- ToDdo: use mySQL seaver's function
 let channel  = "K1:PEM-EX_MAG_X_FLOOR"
 maybexs <- mapM (readFrameV channel) filelist
 let xs = map (fromMaybe (error " no data in the file.")) maybexs
 let ys = DVG.concat xs

-- zs <- kagraDataGet 1124077565 64 "K1:PEM-EX_MAG_X_FLOOR"
-- let ys = fromJust zs



 let fname = "hoge.png"
 let color = [BLUE, RED, PINK]
 let freq  = [(0.1, 1.0), (1.0, 4.0), (4.0, 8.0)]::[(Double, Double)]
-- let gpsend = gpsstart+(fromIntegral nSplit -1)*duration::Double
 let rms   = rmsMon nSplit fs ys freq
 oPlotV Linear LinePoint 1 color (xlabel, ylabel) 0.05 "RMSMon (BLUE:0.1-1Hz, RED:1-4Hz, PINK:4-8Hz)" fname ((0,0),(0,2e-3)) rms
 
 return 0

