
import HasKAL.MonitorUtils.GlitchMon.DBFunction (extractTrigInfoTFSNRSize)
import HasKAL.MonitorUtils.GlitchMon.GlitchPlot (scatterplot'png', scatterplot')
import HasKAL.TimeUtils.GPSfunction (time2gps)
import System.Console.GetOpt
import System.Environment (getArgs)

main = do
  getArgs >>= \args' -> case options args' of
    Right (ss, args) -> case ss of
      [] -> do let setTime = "setGPSTime"
               go setTime args
      _  -> do let setTime = "setLocalTime"
               go setTime args
  where 
    go setTime args = case (length args) of
      6 -> case setTime of
             "setGPSTime" -> do
               let (startgps', stopgps', snrlow', snrhigh', flow', fhigh') = 
                     (head args, args!!1, args!!2, args!!3, args!!4, args!!5)
                   startgps = read startgps' :: Int
                   stopgps = read stopgps' :: Int
                   snrlow = read snrlow' :: Double
                   snrhigh = read snrhigh' :: Double
                   flow = read flow' :: Double
                   fhigh = read fhigh' :: Double
               fromDB <- extractTrigInfoTFSNRSize startgps stopgps snrlow snrhigh flow fhigh
               case fromDB of
                 Just x  -> do 
                   let str_title = "triggered events"
                       str_legend = "events"
                       plotfname = "glitch_GPS-"++startgps'++"_"++stopgps'++".png"
                    in scatterplot'png' str_title str_legend plotfname x 
                 Nothing -> error "no glitch events found"
             "setLocalTime" -> do
               let (startlocal, stoplocal, snrlow', snrhigh', flow', fhigh') = 
                     (head args, args!!1, args!!2, args!!3, args!!4, args!!5)
                   gpsstart = read (time2gps startlocal) ::Int
                   gpsstop  = read (time2gps stoplocal) ::Int
                   snrlow = read snrlow' :: Double
                   snrhigh = read snrhigh' :: Double
                   flow = read flow' :: Double
                   fhigh = read fhigh' :: Double
               fromDB <- extractTrigInfoTFSNRSize gpsstart gpsstop snrlow snrhigh flow fhigh
               case fromDB of
                 Just x  -> do 
                   let str_title = "triggered events"
                       str_legend = "events"
                       plotfname = "glitch_GPS-"++show gpsstart++"_"++show gpsstop++".png"
                    in scatterplot'png' str_title str_legend plotfname x 
                 Nothing -> error "no glitch events found"
 
      _ -> error "Usage: plotGlitchEvent [-l] startGPS[2010-11-11 00:00:00 JST] stopGPS[2010-11-11 00:00:00 JST] SNRlow SNRhigh flow fhigh"




data Flag = LocalTime deriving Show


options :: [String] -> Either String ([Flag],[String])
options args = 
  case getOpt RequireOrder [Option ['l'] ["localtime"] (NoArg LocalTime) "localtime"] args of
    (ss, as, []) -> Right (ss, as)
    (_ ,  _, es) -> Left $ concat es


