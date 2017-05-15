

import Data.Maybe (fromMaybe)
import qualified Data.Vector.Storable as V
import HasKAL.FrameUtils.Function (readFrameV)
import HasKAL.IOUtils.Function
import HasKAL.PlotUtils.HROOT.PlotGraph
import HasKAL.PlotUtils.PlotOption.PlotOptionHROOT
import HasKAL.SpectrumUtils.Function (mapSpectrum)
import HasKAL.SpectrumUtils.SpectrumUtils (gwOnesidedPSDV)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.IO.Unsafe (unsafePerformIO)



main = do
  {-- parameters --}
  (varOpt, varArgs) <- getArgs >>= \optargs ->
    case getOpt Permute options optargs of
      (opt, args, []) -> return (Prelude.foldl (flip id) defaultOptions opt, args)
      (_  ,  _, errs) -> ioError(userError (concat errs ++ usageInfo header options))
        where header = "Usage plotSpectrum [OPTION...] fs t0 dt[s] stdin"

  let ch = head varArgs
      fs = read (varArgs !! 1) :: Double
      t0 = read (varArgs !! 2) :: Double
      dt = read (varArgs !! 3) :: Double
      plotscale = read (optScale varOpt) :: LogOption

  {-- read data --}
  let inputPart = unsafePerformIO $ stdin2vec

  {-- plot parameter --}
  let dtfft = show dt
      title = "#splitline{Spectrum: "++ch++" ("++z++")}{   ("++x++")}"
        where x = "GPS: "++(show t0)++" ~ "++(show (t0+fromIntegral (V.length inputPart-1)/fs))
              z = "dt_{FFT}="++dtfft++"s"

  {-- plot --}
  let xplotPart x =
        case optXPlot varOpt of
          False -> return x
          True -> do
            let snf = mapSpectrum sqrt $ gwOnesidedPSDV x (floor (dt*fs)) fs
            plotXV plotscale Line 1 BLUE ("frequency [Hz]", "[/rHz]") 0.05 title ((0,0),(0,0)) snf
            return x
      plotPart x =
        case optPlot varOpt of
          [] -> return x
          oFile  -> do
            let snf = mapSpectrum sqrt $ gwOnesidedPSDV x (floor (dt*fs)) fs
            plotV plotscale Line 1 BLUE ("frequency [Hz]", "[/rHz]") 0.05 title oFile ((0,0),(0,0)) snf
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
 , optScale    = "LogXY"
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
