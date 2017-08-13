
import qualified Data.Vector.Storable as V
import HasKAL.IOUtils.Function
import HasKAL.TimeUtils.GPSfunction (getCurrentGps)
import qualified Numeric.LinearAlgebra as NL
import System.Console.GetOpt
import System.Environment (getArgs)
import System.IO (stdout, hPutStrLn)

main = do
  (varOpt, varArgs) <- getArgs >>= \optargs ->
    case getOpt Permute options optargs of
      (opt, args,[]) -> return (Prelude.foldl (flip id) defaultOptions opt, args)
      (_  , _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
        where header = "Usage: genRrandomData [-d(--dist)] #data"

  {-- parameters --}
  n <- case (length varArgs) of
    1 -> return (read (head varArgs) :: Int)
    _ -> error "Usage: genRrandomData [-d(--dist)] #data"

  gps <- getCurrentGps >>= \t -> return (read t :: Int)


  case optDist varOpt of
    Nothing -> do let v = NL.randomVector gps NL.Gaussian n
                  mapM_ (\y -> hPutStrLn stdout $ show y) (V.toList v)
    Just x -> case x of
      "Gaussian" -> do let v = NL.randomVector gps NL.Gaussian n
                       mapM_ (\y -> hPutStrLn stdout $ show y) (V.toList v)
      "Uniform" -> do let v = NL.randomVector gps NL.Uniform n
                      mapM_ (\y -> hPutStrLn stdout $ show y) (V.toList v)


data Options = Options
  { optDist     :: Maybe String
  } deriving (Show)

defaultOptions = Options
 { optDist     = Nothing
 }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['d'] ["dist"]
      ( ReqArg (\ f opts -> opts {optDist = Just f}) "DIST")
      "RandDist"
  ]
