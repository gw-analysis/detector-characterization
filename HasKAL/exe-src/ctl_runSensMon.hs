

-- module DailySensMon
-- ( dailySensMon
-- )
-- where
--


import Data.List.Split
import Data.Maybe (fromMaybe)
import qualified Data.Vector.Storable as VS
import HasKAL.ExternalUtils.LIGO.NDS2.Function ( Daq_channel_t (..)
                                               , getChannels
                                               , getData
                                               , selectKeywords
                                               , eliminateKeywords)
import HasKAL.MonitorUtils.SensMon.SensMon
import HasKAL.MonitorUtils.SensMon.Signature
import HasKAL.MonitorUtils.SensMon.Data
import HasKAL.MonitorUtils.SensMon.Function
import HasKAL.PlotUtils.HROOT.PlotGraph3D
import HasKAL.SignalProcessingUtils.Resampling (resampleSV)
import HasKAL.TimeUtils.Function(deformatGPS)
import HasKAL.TimeUtils.GPSfunction (time2gps)
import HasKAL.TimeUtils.Signature
import Numeric.LinearAlgebra.Data (saveMatrix)
import System.Environment (getArgs)
import System.IO.Unsafe (unsafePerformIO)
import System.IO (openFile, stdout, hPutStrLn, hClose,IOMode(..) )
import System.Console.GetOpt


main = do
  (varOpt, varArgs) <- getArgs >>= \optargs ->
    case getOpt Permute options optargs of
      (opt, args, []) -> return (Prelude.foldl (flip id) defaultOptions opt, args)
      (_  ,  _, errs) -> ioError(userError (concat errs ++ usageInfo header options))
       where header = "Usage: runSensMon [OPTION...] \"yyyy-mm-dd hh:mm:ss Locale\" duration fftsec ch"

  (t, duration, fftsecond, ch) <- case length varArgs of
     4 -> return (varArgs!!0, varArgs!!1, varArgs!!2, varArgs!!3)
     _ -> error "Usage: runSensMon [OPTION...] \"yyyy-mm-dd hh:mm:ss Locale\" duration fftsec ch"

  let gps = case optLocal varOpt of
          True  -> read (varArgs!!1) :: Int
          False -> read (time2gps (varArgs!!1)) :: Int

  let ip_nds = "10.68.10.122"
      port   = 8088
      channel = head varArgs
      duration = read (varArgs!!2) :: Int
      chinfo' = selectKeywords [channel] $ getChannels ip_nds port gps
      chinfo | (chinfo'==[]) = Nothing
             | otherwise = Just $ head chinfo'

  maybeVFS <- case chinfo of
    Just x -> do
      let fs' = ch_rate x :: Double
          v'' = getData ip_nds port [(channel,fs')] gps (gps+duration) duration
          v' | v''==Nothing = Nothing
             | otherwise = Just $ head (head (fromMaybe (error "Usage: runSensMon [OPTION...] \"yyyy-mm-dd hh:mm:ss Locale\" duration fftsec ch") v''))
      case v' of
        Nothing -> return Nothing
        Just z -> return $ Just (z,fs')
    Nothing -> return Nothing

  (v, fs) <- case maybeVFS of
    Nothing -> error "no data found"
    Just (v, fs) -> return (v, fs)

  vr <- case (optResample varOpt) of
    []    -> return v
    "1/1" -> return v
    s     -> do let (p,q) = getPQ s
                return $ resampleSV (p,q) fs v

  let duration' = show duration
      fftsecond'= read fftsecond :: Int
      fl = 1/fromIntegral fftsecond'
      fu = fs/2
      p = PlotParam
                { title = "SensMon: " ++ ch
                , filename = ch++"-"++ t ++"_"++duration'++"_SensMon.png"
                , xlabel = "frequency [Hz]"
                , ylabel = "ASD [1/rHz]"
                , zlabel = "count"
                , scale = LogXZ
                , colorbar = COLZ
                }
      nfft = floor fs * fftsecond'
      (outsens, outparam) =  runSensMon vr fs nfft
  case (optPlot varOpt) of
        True -> go (outsens, outparam) (fl, fu) p
        False-> case optXPlot varOpt of
                  True  -> goX (outsens, outparam) (fl, fu) p
                  False-> case optOutput varOpt of
                             False -> error "Usage: runSensMon [OPTION...] \"yyyy-mm-dd hh:mm:ss Locale\" duration fftsec ch"
                             True  -> do let (vt,vf,specgram) = outsens
                                             filename = ch++"-"++ (show t) ++"_"++(show duration)++"_SensMon.dat"
                                         saveMatrix filename "%lf" specgram




data PlotParam = PlotParam
       { title :: String
       , filename :: String
       , xlabel :: String
       , ylabel :: String
       , zlabel :: String
       , scale :: LogOption
       , colorbar :: PlotTypeOption3D
       }

updatePlotParam'title p x = p {title = x}
updatePlotParam'filename p x = p {filename = x}
updatePlotParam'xlabel p x = p {xlabel = x}
updatePlotParam'ylabel p x = p {ylabel = x}
updatePlotParam'scale p x = p {scale = x}
updatePlotParam'colorbar p x = p {colorbar = x}


go :: (SensSpectrum, SensParam) -> (Double, Double) -> PlotParam -> IO ()
go (x, param) (fl, fu) plotparam = do
  let hmin = histmin param
      hmax = histmax param
      title' = title plotparam
      fname' = filename plotparam
      xlabel' = xlabel plotparam
      ylabel' = ylabel plotparam
      zlabel' = zlabel plotparam
      scale' = scale plotparam
      color' = colorbar plotparam
  histgram2dM scale' color' (xlabel', ylabel', zlabel') title' fname' ((fl,fu), (hmin,hmax+1)) x


goX :: (SensSpectrum, SensParam) -> (Double, Double) -> PlotParam -> IO ()
goX (x, param) (fl, fu) plotparam = do
  let hmin = histmin param
      hmax = histmax param
      title' = title plotparam
      fname' = filename plotparam
      xlabel' = xlabel plotparam
      ylabel' = ylabel plotparam
      zlabel' = zlabel plotparam
      scale' = scale plotparam
      color' = colorbar plotparam
  histgram2dMX scale' color' (xlabel', ylabel', zlabel') title' ((fl,fu), (hmin,hmax+1)) x


type RSfactor = String

data Options = Options
 { optResample :: RSfactor
 , optOutput   :: Bool
 , optLocal    :: Bool
 , optPlot     :: Bool
 , optXPlot    :: Bool
 } deriving (Show)


defaultOptions  = Options
  { optResample = []
  , optOutput   = False
  , optLocal    = False
  , optPlot     = False
  , optXPlot    = True
  }


options :: [OptDescr (Options -> Options)]
options =
  [ Option ['r'] ["resample"]
      ( ReqArg (\ pq opts -> opts { optResample = pq}) "P/Q" )
      "resampling factor P/Q"
  , Option ['o'] ["output"]
      ( NoArg (\ opts -> opts {optOutput = True}))
      "output FILE"
  , Option ['l'] ["Localtime"]
      ( NoArg (\ opts -> opts {optLocal = True}))
      "Localtime"
  , Option ['p'] ["plot"]
      ( ReqArg (\p opts -> opts {optPlot = True}) "FILE")
      "plot file"
  , Option ['X'] ["Xplot"]
      ( NoArg (\ opts -> opts {optXPlot = True}))
      "X plot"
  ]


getPQ x = let a = take 2 $ map (\y-> read y :: Int) $ splitOn "/" x
           in (head a,a!!1)


toFile :: FilePath -> [String] -> IO ()
toFile filename xs = do
  handle <- openFile filename WriteMode
  mapM_ (hPutStrLn handle) xs
  hClose handle
