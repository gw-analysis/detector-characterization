

import Data.Maybe (fromMaybe)
import qualified Numeric.LinearAlgebra as NL
import Numeric.LinearAlgebra.Data --(saveMatrix, cols, rows, fromColumns)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.IO (stdout, hPutStrLn)
import System.IO.Unsafe (unsafePerformIO)

import HasKAL.DataBaseUtils.FrameFull.Data
import HasKAL.FrameUtils.FrameUtils (getSamplingFrequency)
import HasKAL.IOUtils.Function (stdin2vecs)
import HasKAL.MonitorUtils.GlitchMon.GlitchMonDAT
import HasKAL.MonitorUtils.GlitchMon.GlitchParam
import HasKAL.MonitorUtils.GlitchMon.PipelineFunction
import HasKAL.Misc.ConfFile (readConfFile)
import qualified HasKAL.PlotUtils.HROOT.PlotGraph3D as H3
import HasKAL.SpectrumUtils.Function (updateMatrixElement,  updateSpectrogramSpec)
import HasKAL.SpectrumUtils.Signature (Spectrum,  Spectrogram)
import qualified HasKAL.WaveUtils.Data as W (WaveData(..), vec2wave)



main = do
  {-- parameters --}
  (varOpt, varArgs) <- getArgs >>= \optargs ->
    case getOpt Permute options optargs of
      (opt, args, []) -> return (Prelude.foldl (flip id) defaultOptions opt, args)
      (_  ,  _, errs) -> ioError(userError (concat errs ++ usageInfo header options))
        where header = "Usage: runClusteringSTDIN [OPTION...] conffile STDIN"

  conffile  <- case (length varArgs) of
    1 -> return (head varArgs)
    _ -> error "Usage: runClusteringSTDIN [OPTION...] conffile STDIN"
  ([cT, element, minN, nN, maxN], [])
    <- readConfFile conffile ["clusterThres","element",
        "minimumClusterNum", "nNeighbor", "maxNtrigg"] []

  let param = GlitchParam
               { segmentLength = 0 :: Int
--               , channel = "K1:LSC-MICH_CTRL_CAL_OUT_DQ"
               , channel = "dummy" --"H1:LOSC-STRAIN"
               , samplingFrequency = 0 :: Double
             -- * whitening
               , traindatlen = 0 :: Double -- [s] must be > 2 * fs/whnFrequencyResolution
               , whnFrequencyResolution = 0 :: Double -- [Hz]
               , whtCoeff = []
               , whnMethod = FrequencyDomain :: WhnMethod
             -- * t-f expression
               , nfrequency = 0 :: Double
               , ntimeSlide = 0 :: Double
             -- * clustering
               , cutoffFractionTFT = 0 :: Double
               , cutoffFractionTFF = 0 :: Double
               , cutoffFreq = 0 :: Double
               , clusterThres = read cT :: Double
               , celement = basePixel5
               , minimumClusterNum = read minN :: Int
               , nNeighbor = read nN :: Int
               , maxNtrigg = read maxN :: Int
             -- * clean data finder
               , cdfInterval = 0 :: Int
               , cdfparameter = cdfp
               , cgps = Nothing
               , reftime = 0
             -- * for debug
               , debugmode = []
               , debugDir = "production"
               }
      cdfp = CDFParam
              { cdf'samplingFrequency = 0 :: Double
              , cdf'cutoffFrequencyLow = 10
              , cdf'cutoffFrequencyHigh = 500
              , cdf'blockSize = 50
              , cdf'fftSize = nfrequency param
              , cdf'chunkSize = traindatlen param
              }

  vs <- stdin2vecs
  let m = fromColumns vs

  maybeCL <- runClusteringDAT param m
  case maybeCL of
    Nothing -> error "Usage: runClusteringSTDIN [OPTION...] conffile STDIN"
    Just (etgSpe, etgID) -> do
      let (etgT,etgF,etgM) = etgSpe
      let retgM = rows etgM
          cetgM = cols etgM
      let zeroMatrix = (retgM >< cetgM) $ replicate (retgM*cetgM) 0.0
      let etgID' = concat etgID
      let tileValues = map (\(x,i)->x) etgID'
      let idValues = map (\(x,i)->fromIntegral i :: Double) etgID'
      let idMM = updateSpectrogramSpec etgSpe $ updateMatrixElement zeroMatrix tileValues idValues
      let title = "cluster plot"
      let plotscale = H3.Linear
      mapM_ (\((x,y),z) -> hPutStrLn stdout $ (show x)++" "++show y++" "++show z) etgID'
      case optPlot varOpt of
        [] -> return ()
        oFile  -> do
          H3.histgram2dM plotscale H3.COLZ ("time index", "frequency index", "id") title oFile ((0,0),(0,0)) idMM
          return ()
      case optXPlot varOpt of
        False -> return ()
        True -> do
          H3.histgram2dMX plotscale H3.COLZ ("time index", "frequency index", "id") title ((0,0),(0,0)) idMM
          return ()


data Options = Options
 { optXPlot    :: Bool
 , optPlot     :: FilePath
 , optScale    :: String
 } deriving (Show)


defaultOptions = Options
 { optXPlot    = True
 , optPlot     = []
 , optScale    = "Linear"
 }


options :: [OptDescr (Options -> Options)]
options =
  [ Option ['X'] ["Xplot"]
      ( NoArg (\ opts -> opts {optXPlot = True}))
      "X plot"
  , Option ['p'] ["plot"]
      ( ReqArg (\p opts -> opts {optPlot = p}) "FILE")
      "plot file"
  , Option ['s'] ["scale"]
      ( ReqArg (\s opts -> opts {optScale = s}) "plot Scale")
      "plot scale"
  ]
