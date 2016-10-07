import Data.Maybe (fromMaybe)
import GlitchMon.Data (PlotOPT(..))
import GlitchMon.DBFunction
import GlitchMon.GlitchPlot (scatterplot'png', scatterplot')
import HasKAL.TimeUtils.GPSfunction (time2gps)
import HasKAL.TimeUtils.Function (deformatGPS)
import HasKAL.PlotUtils.PlotUtils
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
      4 -> case setTime of
             "setGPSTime" -> do
               let (startgps', stopgps', p1', p2') = 
                     (head args, args!!1, args!!2, args!!3)
                   startgps = read startgps' :: Int
                   stopgps = read stopgps' :: Int
                   p1'' = read p1' :: PlotOPT 
                   p2'' = read p2' :: PlotOPT 
                   p1 = setParameter startgps stopgps p1''
                   p2 = setParameter startgps stopgps p2''
                   str_title = "triggered during iKAGRA"
                   str_legend = "events"
                   plotfname = "glitch_scatter_GPS-"++startgps'++"_"++stopgps'++".png"
                in scatter_plot_2d_png str_title str_legend 10 plotfname $ zip p1 p2
             "setLocalTime" -> do
               let (startlocal, stoplocal, p1', p2') = 
                     (head args, args!!1, args!!2, args!!3)
                   startgps = read (time2gps startlocal) ::Int
                   stopgps  = read (time2gps stoplocal) ::Int
                   p1'' = read p1' :: PlotOPT 
                   p2'' = read p2' :: PlotOPT 
                   p1 = setParameter startgps stopgps p1''
                   p2 = setParameter startgps stopgps p2''
                   str_title = "triggered during iKAGRA"
                   str_legend = "events"
                   plotfname = "glitch_scatter_GPS-"++show startgps++"_"++show stopgps++".png"
                in scatter_plot_2d_png str_title str_legend 10 plotfname $ zip p1 p2
 
      _ -> error "Usage: plot2DGlitchParameter [-l] startGPS[2010-11-11 00:00:00 JST] stopGPS[2010-11-11 00:00:00 JST] param1 param2"




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




