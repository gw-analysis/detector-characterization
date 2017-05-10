
import Data.Maybe (fromMaybe)
import qualified Data.Vector.Storable as V
import HasKAL.FrameUtils.FrameUtils (getSamplingFrequency, getGPSTime)
import HasKAL.FrameUtils.Function (readFrameV)
import HasKAL.IOUtils.Function
import HasKAL.TimeUtils.Function (deformatGPS)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.FilePath (takeBaseName)
import System.IO (stdout, hPutStrLn, withFile, IOMode(..))
import System.IO.Unsafe (unsafePerformIO)


main = do
  (varOpt, varArgs) <- getArgs >>= \optargs ->
    case getOpt Permute options optargs of
      (opt, args,[]) -> return (Prelude.foldl (flip id) defaultOptions opt, args)
      (_  , _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
        where header = "Usage: frame2stdout [-t(--withTime), -c(--chachefile)] channel framefile(list)"

  (ch, fname) <- case (length varArgs) of
    2 -> return (head varArgs, varArgs !!1)
    _ -> error "Usage: frame2stdout [-t(--withTime), -c(--chachefile)] ch framefile(list)"

  case optCache varOpt of
    False -> do
      let (t',v) =extractdata ch fname
      case optTime varOpt of
        False -> mapM_ (\x -> hPutStrLn stdout $ show x) v
        True  -> mapM_ (\(t,x) -> hPutStrLn stdout $ (show t)++" "++show x) $ zip t' v
    True -> case optTime varOpt of
      True -> do fnames <- readFile fname >>= \contents -> return $ lines contents
                 mapM_ (gwf2tndat ch) fnames
      False -> do fnames <- readFile fname >>= \contents -> return $ lines contents
                  mapM_ (gwf2dat ch) fnames


gwf2tndat :: String
          -> String
          -> IO ()
gwf2tndat channel file = do
  let (t',v) = extractdata channel file
      outfile = takeBaseName file ++ "_t-dat_" ++ channel ++ ".dat"
  withFile outfile WriteMode $ \handle ->
    mapM_ (\(t,x) -> hPutStrLn handle $ (show t)++" "++show x) $ zip t' v


gwf2dat :: String
        -> String
        -> IO ()
gwf2dat channel file = do
  let (t',v) = extractdata channel file
      outfile = takeBaseName file ++ "_dat_" ++ channel ++ ".dat"
  withFile outfile WriteMode $ \handle ->
    mapM_ (\x -> hPutStrLn handle (show x)) v


extractdata :: String
            -> String
            -> ([Double], [Double])
extractdata ch fname = unsafePerformIO $ do
  readFrameV ch fname >>= \maybex ->
    case maybex of
      Nothing -> error "not valid data."
      Just x -> do
        (t0gpss,t0gpse,tsdt) <- getGPSTime fname >>= \out -> case out of
          Nothing -> error " cannot get GPS time."
          Just out -> return out
        t0 <- return $ deformatGPS (t0gpss, t0gpse)
        fs <- getSamplingFrequency fname ch >>= \out -> case out of
          Nothing -> error " cannot get sample rate."
          Just out -> return out
        let t' = [t0,t0+1/fs..t0+fromIntegral (V.length x-1)/fs]
        return (t',V.toList x)



data Options = Options
  { optTime :: Bool
  , optCache :: Bool
  } deriving (Show)


defaultOptions  = Options
  { optTime = False
  , optCache = False
  }


options :: [OptDescr (Options -> Options)]
options =
  [ Option ['t'] ["withTime"]
      ( NoArg (\ opts -> opts {optTime = True}))
      "Usage: frame2stdout [-t(--withTime), -c(--chachefile)] ch framefile(list)"
  , Option ['c'] ["cachefile"]
      ( NoArg (\ opts -> opts {optCache = True}))
      "Usage: frame2stdout [-t(--withTime), -c(--chachefile)] ch framefile(list)"
  ]
