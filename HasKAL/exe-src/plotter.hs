
import Control.Concurrent (forkIO)
import Data.Maybe (fromMaybe)
import qualified Data.Vector.Storable as V
import HasKAL.FrameUtils.Function (readFrameV)
import HasKAL.IOUtils.Function
import HasKAL.PlotUtils.HROOT.PlotGraph
import System.Console.GetOpt
import System.Environment (getArgs)
import System.IO (stdout, hPutStrLn)
import System.IO.Unsafe (unsafePerformIO)


main = do
  {-- parameters --}
  (varOpt, varArgs) <- getArgs >>= \optargs ->
     case getOpt Permute options optargs of
       (opt, args, []) -> return (Prelude.foldl (flip id) defaultOptions opt, args)
       (_  ,  _, errs) -> ioError(userError (concat errs ++ usageInfo header options))
          where header = "Usage plotter [OPTION...] ch fs t0 stdin"

  case (length varArgs == 3) of
    False -> error "Usage plotter [OPTION...] ch fs t0 stdin"
    True ->  do
      let ch = head varArgs
          fs = read (varArgs !! 1) :: Double
          t0 = read (varArgs !! 2) :: Double


      let inputPart = unsafePerformIO $ stdin2vec
          xplotPart x = case optXPlot varOpt of
                          False -> return x
                          True -> do
                            let title = ch
                                tv = (V.fromList [t0,t0+1/fs..t0+(fromIntegral (V.length x-1))/fs], x)
--                        forkIO $ plotXV Linear Line 1 BLUE ("x", "y") 0.05 title ((0,0),(0,0)) tv
--                        return x
                            plotXV Linear Line 1 BLUE (optXlabel varOpt, optYlabel varOpt) 0.05 title ((0,0),(0,0)) tv
                            return x
          plotPart x = case optPlot varOpt of
                         [] -> return x
                         f  -> do
                           let title = ch
                               tv = (V.fromList [t0,t0+1/fs..t0+(fromIntegral (V.length x-1))/fs], x)
                           plotV Linear Line 1 BLUE (optXlabel varOpt, optYlabel varOpt) 0.05 title f ((0,0),(0,0)) tv
                           return x
      x1 <- xplotPart $ inputPart
      plotPart x1



data Options = Options
  { optXPlot    :: Bool
  , optPlot     :: FilePath
  , optXlabel   :: String
  , optYlabel   :: String
  } deriving (Show)


defaultOptions  = Options
  { optXPlot    = False
  , optPlot     = []
  , optXlabel   = "x"
  , optYlabel   = "y"
  }


options :: [OptDescr (Options -> Options)]
options =
  [ Option ['X'] ["Xplot"]
      ( NoArg (\ opts -> opts {optXPlot = True}))
      "X plot"
  , Option ['p'] ["plot"]
      ( ReqArg (\p opts -> opts {optPlot = p}) "FILE")
      "plot file"
  , Option ['x'] ["xlabel"]
      ( ReqArg (\x opts -> opts {optXlabel = x}) "X label")
      "X label"
  , Option ['y'] ["ylabel"]
      ( ReqArg (\y opts -> opts {optYlabel = y}) "Y label")
      "Y label"
  ]
