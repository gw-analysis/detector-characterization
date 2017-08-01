import qualified Data.Vector.Generic as DVG
import System.Console.GetOpt
import System.Environment (getArgs)
import System.IO (stdout, hPutStrLn)
import HasKAL.IOUtils.Function (stdin2vec)
import HasKAL.TimeUtils.GPSfunction (time2gps)
import HasKAL.TimeUtils.Function(deformatGPS,formatGPS)

import HasKAL.PlotUtils.HROOT.PlotGraph
import HasKAL.SpectrumUtils.Signature
import HasKAL.MonitorUtils.RMSMon.RMSMon (rmsMonWaveData)

--import HasKAL.DataBaseUtils.XEndEnv.Function (kagraWaveDataGet0)
import HasKAL.WaveUtils.Data (WaveData(..),mkWaveData)
import qualified Data.Vector.Storable as V
import HasKAL.DetectorUtils.Detector

{-- memo
    running time (24hour data) : ~2m30s
--}

-- TODO : how we get unit of y-axis?

main = do
 (varOpt, varArgs) <- getArgs >>= \optargs ->
   case getOpt Permute options optargs of
     (opt, args,[]) -> return (Prelude.foldl (flip id) defaultOptions opt, args)
     (_  , _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
       where header = "Usage: BLRMS [-t(--withTime)] t0 fs fftsec channel f1low f1high STDIN"

 (t0, fs', fftsec', channel, f1low, f1high) <- case length varArgs of
     6 -> return (varArgs!!0, varArgs!!1, varArgs!!2, varArgs!!3, varArgs!!4, varArgs!!5)
     _ ->  error "Usage: BLRMS [-t(--withTime)] t0 fs fftsec channel f1low f1high STDIN"


 {-- parameters --}
 dat <- stdin2vec
 let gps = read t0 :: Double
     fs = read fs' :: Double
     startGPS = formatGPS gps
     stopGPS = formatGPS (gps + fromIntegral (V.length dat)/fs)

 {-- for RMSMon --}
 let f1band = ((read f1low::Double), (read f1high::Double))
     freq  = [f1band] :: [(Double, Double)]
     fftsec = read fftsec' :: Double

 let wd = mkWaveData General "Temp" fs startGPS stopGPS dat

 {-- for RMSMon --}
 let nmon = floor (fftsec * (samplingFrequency wd)) ::Int -- 86400s / 96[chunk] = 900[s]
 let rms  = head $ rmsMonWaveData nmon freq wd

 case optTime varOpt of
   False -> do
     mapM_ (\y -> hPutStrLn stdout $ show y) (V.toList (snd rms))
   True  -> do
     mapM_ (\(x,y) -> hPutStrLn stdout $ (show x)++" "++show y)
              $ zip (V.toList (fst rms)) (V.toList (snd rms))


data Options = Options
   { optTime :: Bool
   } deriving (Show)


defaultOptions  = Options
   { optTime = False
   }


options :: [OptDescr (Options -> Options)]
options =
   [ Option ['t'] ["withTime"]
       ( NoArg (\ opts -> opts {optTime = True}))
       "Usage: BLRMS [-t(--withTime)] t0 fs fftsec channel f1low f1high STDIN"
   ]
