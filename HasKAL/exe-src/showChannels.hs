
import Data.Int (Int32)
import HasKAL.DataBaseUtils.FrameFull.Function (kagraDataGPS)
import HasKAL.FrameUtils.FrameUtils (getChannelList)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.IO (stdout, hPutStrLn)

main = do
  (varOpt, varArgs) <- getArgs >>= \optargs ->
    case getOpt Permute options optargs of
      (opt, args,[]) -> return (Prelude.foldl (flip id) defaultOptions opt, args)
      (_  , _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
        where header = "Usage: showChannels [OPTION...] file or showChannels gps..."


  case optFile varOpt of
    f -> getChannelList f >>= \maybech -> case maybech of
           Nothing -> error "no channel at present."
           Just x -> mapM_ (\(y, z)-> hPutStrLn stdout y) x
    [] -> do let gpsstr = head varArgs
             kagraDataGPS (read gpsstr :: Int32) >>= \maybefiles -> case maybefiles of
               Nothing -> error "no file found."
               Just files -> do
                 let fname = head files
                 getChannelList fname >>= \maybech -> case maybech of
                   Nothing -> error "no channel at present."
                   Just x -> mapM_ (\(y, z)-> hPutStrLn stdout y) x


data Options = Options
  { optFile     :: FilePath
  } deriving (Show)

defaultOptions = Options
 { optFile     = []
 }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['f'] ["file"]
      ( ReqArg (\ f opts -> opts {optFile = f}) "FILE")
      "frame file"
  ] 

