
import qualified Data.Vector.Generic as DVG
import qualified Numeric.LinearAlgebra as NLA
import Data.Maybe (fromMaybe, fromJust)

import HasKAL.PlotUtils.HROOT.PlotGraph
import HasKAL.FrameUtils.Function (readFrameV)
import HasKAL.SpectrumUtils.SpectrumUtils (gwpsdV)
import HasKAL.SpectrumUtils.Signature
import HasKAL.MonitorUtils.RMSMon.RMSMon (rmsMon)

import ReadFiles

--import HasKAL.TimeUtils.GPSfunction
--import HasKAL.DataBaseUtils.Function (kagraDataGet)
-- error
--zs <- kagraDataGet 1124077565 64 "K1:PEM-EX_MAG_X_FLOOR"

main = do

 {-- open frame file --}
 let fs = 2048::Double
 let nfile = 100 :: Int -- you can change
     filelist = take nfile testFiles

 let totalduration  = 32 * nfile :: Int
     nSplit = 20 :: Int -- you can change
--     iduration = totalduration `div` nSplit :: Int
--     duration = fromIntegral iduration

-- let gps = 1124077533::Double
 let gpsstart = 77533::Double


 -- ToDdo: use mySQL seaver's function
 let channel  = "K1:PEM-EX_MAG_X_FLOOR"
 maybexs <- mapM (readFrameV channel) filelist
 let xs = map (fromMaybe (error " no data in the file.")) maybexs
 let ys = DVG.concat xs
 --print $ DVG.take 100 ys
 --print $ DVG.length ys


 let fname = "hoge.png"
 let color = [BLUE, RED, PINK]
 let freq  = [(0.1, 1.0), (1.0, 4.0), (4.0, 8.0)]::[(Double, Double)]
-- let gpsend = gpsstart+(fromIntegral nSplit -1)*duration::Double
 let rms   = rmsMon nSplit gpsstart fs ys freq
 oPlotV Linear LinePoint 1 color ("GPS[sec]", "Voltage[V]") 0.05 "RMSMon (BLUE:0.1-1Hz, RED:1-4Hz, PINK:4-8Hz)" fname ((0,0),(1e-4,1e-3)) rms
 
 return 0

