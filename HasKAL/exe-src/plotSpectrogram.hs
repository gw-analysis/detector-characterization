

import Data.Maybe (fromMaybe)
import qualified Data.Vector.Storable as V
import HasKAL.FrameUtils.Function (readFrameV)
import HasKAL.IOUtils.Function
import HasKAL.PlotUtils.HROOT.PlotGraph3D
import HasKAL.PlotUtils.PlotOption.PlotOptionHROOT
import HasKAL.SpectrumUtils.Function (mapSpectrogram)
import HasKAL.SpectrumUtils.SpectrumUtils (gwspectrogramV)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.IO.Unsafe (unsafePerformIO)



main = do
  {-- parameters --}
  (varOpt, varArgs) <- getArgs >>= \optargs ->
    case getOpt Permute options optargs of
      (opt, args, []) -> return (Prelude.foldl (flip id) defaultOptions opt, args)
      (_  ,  _, errs) -> ioError(userError (concat errs ++ usageInfo header options))
        where header = "Usage plotSpectrogram [OPTION...] fs t0 dt[s] overwrappedTime[s]"

  let ch = head varArgs
      fs = read (varArgs !! 1) :: Double
      t0 = read (varArgs !! 2) :: Double
      dt = read (varArgs !! 3) :: Double
      ot = read (varArgs !! 4) :: Double
      plotscale = read (optScale varOpt) :: LogOption


  {-- read data --}
  let inputPart' = unsafePerformIO $ stdin2vecs
  let inputPart | length inputPart' == 1 = head inputPart'
                | length inputPart' >= 2 = inputPart' !! 1
                | otherwise = error "unknown input"

  {-- plot parameter --}
  let dtfft = show dt
      title = "#splitline{Spectrogram: "++ch++" ("++z++")}{   ("++x++")}"
        where x = "GPS: "++(show t0)++" ~ "++(show (t0+fromIntegral (V.length inputPart-1)/fs))
              z = "dt_{FFT}="++dtfft++"s"

  {-- plot --}
  let xplotPart x =
        case optXPlot varOpt of
          False -> return x
          True -> do
            let y = mapSpectrogram sqrt $ gwspectrogramV  (floor (ot*fs)) (floor (dt*fs)) fs x
            histgram2dMX plotscale COLZ ("time", "frequency [Hz]", "[1/rHz]") title ((0,0),(0,0)) y
            return x
      plotPart x =
        case optPlot varOpt of
          [] -> return x
          oFile  -> do
            let y = mapSpectrogram sqrt $ gwspectrogramV  (floor (ot*fs)) (floor (dt*fs)) fs x
            histgram2dM plotscale COLZ ("time", "frequency [Hz]", "[1/rHz]") title oFile ((0,0),(0,0)) y
            return x

  result1 <- xplotPart inputPart
  plotPart result1



data Options = Options
 { optXPlot    :: Bool
 , optPlot     :: FilePath
 , optScale    :: String
 } deriving (Show)


defaultOptions = Options
 { optXPlot    = True
 , optPlot     = []
 , optScale    = "LogYZ"
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
