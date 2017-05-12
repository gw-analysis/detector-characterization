
import Data.Maybe (fromMaybe)
import qualified Data.Vector.Storable as V
import HasKAL.FrameUtils.Function (readFrameV)
import HasKAL.IOUtils.Function
import HasKAL.SignalProcessingUtils.FilterType
import qualified HasKAL.SignalProcessingUtils.ButterWorth as B
import qualified HasKAL.SignalProcessingUtils.Chebyshev as C
import HasKAL.SignalProcessingUtils.FilterX
import System.Console.GetOpt
import System.Environment (getArgs)
import System.IO (stdout, hPutStrLn)
import System.IO.Unsafe (unsafePerformIO)


main = do
  (varOpt, varArgs) <- getArgs >>= \optargs ->
    case getOpt Permute options optargs of
      (opt, args,[]) -> return (Prelude.foldl (flip id) defaultOptions opt, args)
      (_  , _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
        where header = "Usage: filter [-t(--withTime)] channel fs fc ftype stdin"

  {-- parameters --}
  (ch,fs',fc',ftype') <- case (length varArgs) of
    4 -> return (head varArgs, varArgs!!1, varArgs!!2, varArgs!!3)
    _ -> error "Usage: filter [-t(--withTime)] channel fs fc ftype stdin"

  let fs = read fs'       :: Double
      fc = read fc'       :: Double
      ftype = read ftype' :: FilterType

  let filterPart v = case ftype of
        Low -> let lpf = C.chebyshev1 6 1 fs fcp Low
                   fcp = 2*fs*tan (2*pi*fc/fs/2)/(2*pi)
                in filtfilt0 lpf v
        High -> let hph = C.chebyshev1 6 1 fs fcp High
                    fcp = 2*fs*tan (2*pi*fc/fs/2)/(2*pi)
                 in filtfilt0 hph v
  case optTime varOpt of
    False -> do
      let inputPart = unsafePerformIO $ stdin2vec
      mapM_ (\y -> hPutStrLn stdout $ show y) (V.toList (filterPart inputPart))
    True  -> do
      let inputPart = unsafePerformIO $ stdin2vecs
      mapM_ (\(x,y) -> hPutStrLn stdout $ (show x)++" "++show y)
               $ zip (V.toList (head inputPart)) (V.toList (filterPart (inputPart!!1)))


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
      "Usage: filter [-t(--withTime)] channel fs fc ftype stdin"
  ]
