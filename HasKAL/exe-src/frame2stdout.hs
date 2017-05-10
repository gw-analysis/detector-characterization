
import Data.Maybe (fromMaybe)
import qualified Data.Vector.Storable as V
import HasKAL.FrameUtils.FrameUtils (getSamplingFrequency, getGPSTime)
import HasKAL.FrameUtils.Function (readFrameV)
import HasKAL.IOUtils.Function
import HasKAL.TimeUtils.Function (deformatGPS)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.IO (stdout, hPutStrLn)
import System.IO.Unsafe (unsafePerformIO)

main = do
  (varOpt, varArgs) <- getArgs >>= \optargs ->
    case getOpt Permute options optargs of
      (opt, args,[]) -> return (Prelude.foldl (flip id) defaultOptions opt, args)
      (_  , _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
        where header = "Usage: frame2stdout [-t(--withTime)] channel framefile"

  (ch, fname) <- case (length varArgs) of
    2 -> return (head varArgs, varArgs !!1)
    _ -> error "Usage: frame2stdout [-t(--withTime)] ch framefile"

  (t',v) <- readFrameV ch fname >>= \maybex ->
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
  case optTime varOpt of
    False -> mapM_ (\x -> hPutStrLn stdout $ show x) v
    True  -> mapM_ (\(t,x) -> hPutStrLn stdout $ (show t)++" "++show x) $ zip t' v

data Options = Options
  { optTime :: Bool
  } deriving (Show)


defaultOptions  = Options
  { optTime = False
  }


options :: [OptDescr (Options -> Options)]
options =
  [ Option ['t'] ["withTime"]
      ( NoArg (\ opts -> opts {optTime = True}))
      "Usage: frame2stdout [-t(--withTime)] ch framefile"
  ]
