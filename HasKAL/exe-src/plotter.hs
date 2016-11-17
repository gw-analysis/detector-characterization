
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
  let ch = head varArgs
      fs = read (varArgs !! 1) :: Double
      t0 = read (varArgs !! 2) :: Double


  let inputPart = case optInput varOpt of
                    Just "stdin" -> unsafePerformIO $ stdin2vec
                    Just f -> let ch = Prelude.head varArgs
                               in fromMaybe (error "cannot read data.") (unsafePerformIO $ readFrameV ch f)
                    Nothing -> error "cannot read data."
      xplotPart x = case optXPlot varOpt of
                      False -> return x
                      True -> do
                        let title = ch
                            tv = (V.fromList [t0,t0+1/fs..t0+(fromIntegral (V.length x-1))/fs], x)
                        forkIO $ plotXV Linear Line 1 BLUE ("x", "y") 0.05 title ((0,0),(0,0)) tv
                        return x
      plotPart x = case optPlot varOpt of
                     [] -> return x
                     f  -> do
                       let title = ch
                           tv = (V.fromList [t0,t0+1/fs..t0+(fromIntegral (V.length x-1))/fs], x)
                       forkIO $ plotV Linear Line 1 BLUE ("x", "y") 0.05 title f ((0,0),(0,0)) tv
                       return x
  x1 <- xplotPart $ inputPart
  plotPart x1



data Options = Options
  { optInput    :: Maybe FilePath
  , optXPlot    :: Bool
  , optPlot     :: FilePath
  } deriving (Show)


defaultOptions  = Options
  { optInput    = Nothing
  , optXPlot    = False
  , optPlot     = []
  }


options :: [OptDescr (Options -> Options)]
options = 
  [ Option ['i'] ["input"]
      ( OptArg ((\ f opts -> opts {optInput = Just f}) . fromMaybe "stdin") "FILE" )
      "input FILE"
  , Option ['X'] ["Xplot"]
      ( NoArg (\ opts -> opts {optXPlot = True}))
      "X plot"
  , Option ['p'] ["plot"]
      ( ReqArg (\p opts -> opts {optPlot = p}) "FILE")
      "plot file"
  ]
