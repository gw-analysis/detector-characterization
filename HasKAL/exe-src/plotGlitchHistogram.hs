import Data.Maybe (fromMaybe)
import HasKAL.MonitorUtils.GlitchMon.Data (PlotOPT(..))
import HasKAL.MonitorUtils.GlitchMon.DBFunction
import HasKAL.MonitorUtils.GlitchMon.GlitchPlot (scatterplot'png', scatterplot')
import HasKAL.TimeUtils.GPSfunction (time2gps)
import HasKAL.TimeUtils.Function (deformatGPS)
import HasKAL.PlotUtils.PlotUtils
import HasKAL.PlotUtils.HROOT.Histogram (hist)
import HasKAL.PlotUtils.PlotOption.PlotOptionHROOT (LogOption(Linear, LogX, LogY, LogXY))
import System.Console.GetOpt
import System.Environment (getArgs)
import System.IO.Unsafe (unsafePerformIO)

main = do
  getArgs >>= \args' -> case options args' of
    Right (ss, args) -> case ss of
      [] -> do let setTime = "setGPSTime"
               go setTime args
      _  -> do let setTime = "setLocalTime"
               go setTime args
  where 
    go setTime args = case (length args) of
      7 -> case setTime of
             "setGPSTime" -> do
               let (startgps', stopgps', p1', p2',n',xmin',xmax') = 
                     (head args, args!!1, args!!2, args!!3, args!!4, args!!5, args!!6)
                   startgps = read startgps' :: Int
                   stopgps = read stopgps' :: Int
                   p1'' = read p1' :: PlotOPT 
                   p2 = read p2' :: LogOption
                   p1 = setParameter startgps stopgps p1''
                   n = read n' :: Int
                   xmin = read xmin' :: Double
                   xmax = read xmax' :: Double 
                   str_title = "Histogram: SNR of triggered evnts"
                   plotfname = "glitch_"++p1'++"_hist_GPS-"++startgps'++"_"++stopgps'++".png"
                in hist p2 ("SNR","N") str_title plotfname (n,xmin,xmax) p1
             "setLocalTime" -> do
               let (startlocal, stoplocal, p1', p2', n', xmin', xmax') = 
                     (head args, args!!1, args!!2, args!!3, args!!4, args!!5, args!!6)
                   startgps = read (time2gps startlocal) ::Int
                   stopgps  = read (time2gps stoplocal) ::Int
                   startgps' = show startgps
                   stopgps' = show stopgps
                   p1'' = read p1' :: PlotOPT 
                   p2 = read p2' :: LogOption
                   p1 = setParameter startgps stopgps p1''
                   n = read n' :: Int
                   xmin = read xmin' :: Double
                   xmax = read xmax' :: Double 
                   str_title = "Histogram: SNR of triggered evnts"
                   plotfname = "glitch_"++p1'++"_hist_GPS-"++startgps'++"_"++stopgps'++".png"
                in hist p2 ("SNR","N") str_title plotfname (n,xmin,xmax) p1
 
      _ -> error "Usage: plotGlitchHistogram [-l] startGPS[2010-11-11 00:00:00 JST] stopGPS[2010-11-11 00:00:00 JST] GlitchParam PlotOpt(Linear,LogX,LogY,LogXY) #bin xmin xmax "




data Flag = LocalTime deriving Show

options :: [String] -> Either String ([Flag],[String])
options args = 
  case getOpt RequireOrder [Option ['l'] ["localtime"] (NoArg LocalTime) "localtime"] args of
    (ss, as, []) -> Right (ss, as)
    (_ ,  _, es) -> Left $ concat es


setParameter :: Int -> Int -> PlotOPT -> [Double]
setParameter gpsstart gpsstop s
  | s == GPS = let gps = fromMaybe (error "data not found.") $ unsafePerformIO $ extractTrigGPS gpsstart gpsstop 
                in map (deformatGPS . \(x,y)-> (floor x,floor y)) gps 
  | s == CentralFrequency = fromMaybe (error "data not found.") $ unsafePerformIO $ extractTrigCentralFrequency gpsstart gpsstop
  | s == SNR = fromMaybe (error "data not found.") $ unsafePerformIO $ extractTrigSNR gpsstart gpsstop
  | s == DQFlag = fromMaybe (error "data not found.") $ unsafePerformIO $ extractTrigDQFlag gpsstart gpsstop
  | s == Significance = fromMaybe (error "data not found.") $ unsafePerformIO $ extractTrigSignificance gpsstart gpsstop
  | s == Size  = fromMaybe (error "data not found.") $ unsafePerformIO $ extractTrigSize gpsstart gpsstop
  | s == Energy  = fromMaybe (error "data not found.") $ unsafePerformIO $ extractTrigEnergy gpsstart gpsstop
  | s == Duration = fromMaybe (error "data not found.") $ unsafePerformIO $ extractTrigDuration gpsstart gpsstop
  | s == CentralGPS = fromMaybe (error "data not found.") $ unsafePerformIO $ extractTrigCGPS gpsstart gpsstop




