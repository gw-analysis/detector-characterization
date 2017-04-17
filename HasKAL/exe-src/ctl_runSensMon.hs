

-- module DailySensMon
-- ( dailySensMon
-- )
-- where
--



import Data.Maybe (fromMaybe)
import qualified Data.Vector.Storable as VS
import HasKAL.DataBaseUtils.FrameFull.Function(kagraDataGet, kagraDataFind)
import HasKAL.FrameUtils.FrameUtils (getSamplingFrequency)
import HasKAL.MonitorUtils.SensMon.SensMon
import HasKAL.MonitorUtils.SensMon.Signature
import HasKAL.MonitorUtils.SensMon.Data
import HasKAL.MonitorUtils.SensMon.Function
import HasKAL.PlotUtils.HROOT.PlotGraph3D
import HasKAL.TimeUtils.Function(deformatGPS)
import HasKAL.TimeUtils.GPSfunction (time2gps)
import HasKAL.TimeUtils.Signature
import System.Environment (getArgs)
import System.IO.Unsafe (unsafePerformIO)


main = do
  (varOpt, varArgs) <- getArgs >>= \optargs ->
    case getOpt Permute options optargs of
      (opt, args, []) -> return (Prelude.foldl (flip id) defaultOptions opt, args)
      (_  ,  _, errs) -> ioError(userError (concat errs ++ usageInfo header options))
       where header = "Usage: runSensMon [OPTION...] channel \"yyyy-mm-dd hh:mm:ss Locale\" duration fftsec"

  (t, duration, fftsecond, ch) <- case length varArgs of
     4 -> return (args!!0, args!!1, args!!2, args!!3)
     _ -> error "Usage: runSensMon \"yyyy-mm-dd hh:mm:ss Locale\" duration fftsec ch"

  let gps = case optLocal varOpt of
          True  -> read (varArgs!!1) :: Int
          False -> read (time2gps (varArgs!!1)) :: Int

  let ip_nds = "10.68.10.122"
      port   = 8088
      channel = head varArgs
      duration = read (varArgs!!2) :: Int
      chinfo' = selectKeywords [channel] $ getChannels ip_nds port gps
      chinfo | chinfo' = [] = Nothing
             | otherwise = Just $ head chinfo'

  maybeVFS <- case chinfo of
    Just x -> do
      let fs' = ch_rate x :: Double
          v'' = getData ip_nds port [(channel,fs')] gps (gps+duration) duration
          v' | v''==Nothing = Nothing
             | v''==Just y = Just $ head (head y)
             | otherwise = Nothing
      case v' of
        Nothing -> return Nothing
        Just z -> return $ Just (z,fs')
    Nothing -> return Nothing

  case maybeVFS of
    Nothing -> return ()
    Just (v,fs) -> do
      let duration' = read duration :: Int
          fftsecond'= read fftsecond :: Int
          fl = 1/fromIntegral fftsecond'
          fu = fs/2
          p = PlotParam
                { title = "SensMon: " ++ ch
                , filename = ch++"-"++ t ++"_"++duration++"_SensMon.png"
                , xlabel = "frequency [Hz]"
                , ylabel = "ASD [1/rHz]"
                , zlabel = "count"
                , scale = LogXZ
                , colorbar = COLZ
                }
          nfft = floor fs * fftsecond'
          (outsens, outparam) =  runSensMon v fs nfft
      case
      go v duration' fftsecond' (ch, fs) (fl, fu) p


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


go :: VS.Vector Double -> Int -> Int -> (String, Double) -> (Double, Double) -> PlotParam -> IO ()
go v duration fftsecond (ch, fs) (fl, fu) plotparam = do
  let nfft = floor fs * fftsecond
      (x, param) =  runSensMon v fs nfft

      hmin = histmin param
      hmax = histmax param
      title' = title plotparam
      fname' = filename plotparam
      xlabel' = xlabel plotparam
      ylabel' = ylabel plotparam
      zlabel' = zlabel plotparam
      scale' = scale plotparam
      color' = colorbar plotparam
  histgram2dM scale' color' (xlabel', ylabel', zlabel') title' fname' ((fl,fu), (hmin,hmax+1)) x


type RSfactor = String

data Options = Options
 { optResample :: RSfactor
 , optOutput   :: Maybe FilePath
 , optLocal    :: Bool
 , optPlot     :: Maybe FilePath
 } deriving (Show)


defaultOptions  = Options
  { optResample = []
  , optOutput   = Nothing
  , optLocal    = False
  , optPlot     = Nothing
  }


options :: [OptDescr (Options -> Options)]
options =
  [ Option ['r'] ["resample"]
      ( ReqArg (\ pq opts -> opts { optResample = pq}) "P/Q" )
      "resampling factor P/Q"
  , Option ['o'] ["output"]
      ( OptArg ((\ f opts -> opts {optOutput = Just f}) . fromMaybe "stdout") "FILE" )
      "output FILE"
  , Option ['l'] ["Localtime"]
      ( NoArg (\ opts -> opts {optLocal = True}))
      "Localtime"
  , Option ['p'] ["plot"]
      ( ReqArg (\p opts -> opts {optPlot = Just p}) "FILE")
      "plot file"
  ]
