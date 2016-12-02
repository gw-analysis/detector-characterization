

import Data.Maybe (fromMaybe)
import qualified Data.Vector.Storable as V
import HasKAL.FrameUtils.Function (readFrameV)
import HasKAL.IOUtils.Function
import HasKAL.PlotUtils.HROOT.PlotGraph3D
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
  let ch = head varArgs
      fs = read (varArgs !! 1) :: Double
      t0 = read (varArgs !! 2) :: Double
      dt = read (varArgs !! 3) :: Double
      ot = read (varArgs !! 4) :: Double

  {-- read data --}
  let inputPart = unsafePerformIO $ stdin2vec

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
            histgram2dMX LogYZ COLZ (xlabel, "frequency [Hz]", "[1/rHz]") title ((0,0),(0,0)) y
            return x
      plotPart x = 
        case optPlot varOpt of
          [] -> return x
          oFile  -> do
            let y = mapSpectrogram sqrt $ gwspectrogramV  (floor (ot*fs)) (floor (dt*fs)) fs x
            histgram2dM LogYZ COLZ (xlabel, "frequency [Hz]", "[1/rHz]") title oFile ((0,0),(0,0)) y
           return x

  result1 <- xplotPart inputPart
  plotPart result1



data Options = Options
 { optXPlot    :: Bool
 , optPlot     :: FilePath
 } deriving (Show)


defaultOptions = Options
 { optXPlot    = True
 , optPlot     = []
 }


options :: [OptDescr (Options -> Options)]
options =
  [ Option ['X'] ["Xplot"]
      ( NoArg (\ opts -> opts {optXPlot = True}))
      "X plot"
  , Option ['p'] ["plot"]
      ( ReqArg (\p opts -> opts {optPlot = p}) "FILE")
      "plot file"
  ]




