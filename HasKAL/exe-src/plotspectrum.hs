

import Data.Maybe (fromMaybe)
import qualified Data.Vector.Storable as V
import HasKAL.FrameUtils.Function (readFrameV)
import HasKAL.IOUtils.Function
import HasKAL.PlotUtils.HROOT.PlotGraph
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
  let ch = head varArgs
      fs = read (varArgs !! 1) :: Double
      t0 = read (varArgs !! 2) :: Double
      dt = read (varArgs !! 3) :: Double

  {-- read data --}
  let inputPart = 
        case optInput varOpt of
          Just "stdin" -> unsafePerformIO $ stdin2vec
          Just f -> 
            let ch = Prelude.head varArgs
             in fromMaybe (error "cannot read data.") (unsafePerformIO $ readFrameV ch f)

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
            plotXV LogXY Line 1 BLUE ("frequency [Hz]", "[/rHz]") 0.05 title ((0,0),(0,0)) snf
            return x
      plotPart x = 
        case optPlot varOpt of
          [] -> return x
          oFile  -> do
            let snf = mapSpectrum sqrt $ gwOnesidedPSDV x (floor (dt*fs)) fs
            plotV LogXY Line 1 BLUE ("frequency [Hz]", "[/rHz]") 0.05 title oFile ((0,0),(0,0)) snf
            return x

  result1 <- xplotPart inputPart
  plotPart result1



data Options = Options
 { optInput    :: Maybe FilePath
 , optXPlot    :: Bool
 , optPlot     :: FilePath
 } deriving (Show)


defaultOptions = Options
 { optInput    = Nothing
 , optXPlot    = True
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




