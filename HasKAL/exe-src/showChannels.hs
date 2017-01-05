
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
    Just f -> getChannelList f >>= \maybech -> case maybech of
           Nothing -> error "no channel at present."
           Just x -> mapM_ (\(y, z)-> hPutStrLn stdout y) x
    Nothing -> case length varArgs of
          5 -> do let gpsstr = head varArgs
                  kagraDataGPS (read gpsstr :: Int32) >>= \maybefiles -> case maybefiles of
                   Nothing -> error "no file found."
                   Just files -> do
                    let fname = head files
                    getChannelList fname >>= \maybech -> case maybech of
                     Nothing -> error "no channel at present."
                     Just x -> mapM_ (\(y, z)-> hPutStrLn stdout y) x
          _ -> error "Usage: showChannels gps[s]"

data Options = Options
  { optFile     :: Maybe FilePath
  } deriving (Show)

defaultOptions = Options
 { optFile     = Nothing
 }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['f'] ["file"]
      ( ReqArg (\ f opts -> opts {optFile = Just f}) "FILE")
      "frame file"
  ] 

