import Data.List.Split
import Data.Maybe (fromMaybe)
import qualified Data.Vector.Storable as V
import HasKAL.FrameUtils.Function (readFrameV)
import HasKAL.IOUtils.Function
import HasKAL.SignalProcessingUtils.Resampling (resampleSV)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.IO (stdout, hPutStrLn)
import System.IO.Unsafe (unsafePerformIO)


main = do
  (varOpt, varArgs) <- getArgs >>= \optargs ->
    case getOpt Permute options optargs of
      (opt, args,[]) -> return (Prelude.foldl (flip id) defaultOptions opt, args)
      (_  , _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
        where header = "Usage: resampler [-t(--withTime)] channel P/Q fs stdin"

  {-- parameters --}
  (ch, pq, fs') <- case length varArgs of
    3 -> return (head varArgs, varArgs!!1, varArgs!!2)
    _ -> error "Usage: resampler [-t(--withTime)] channel P/Q fs stdin"
  let (p,q) = getPQ pq ::(Int,Int)
      fs = read fs' :: Double

  case optTime varOpt of
    False -> do let inputPart = unsafePerformIO $ stdin2vec
                    resamplePart v = resampleSV (p,q) fs v
                mapM_ (\y -> hPutStrLn stdout $ show y) (V.toList (resamplePart inputPart))
    True  -> do let inputPart = unsafePerformIO $ stdin2vecs
                    resamplePart v = resampleSV (p,q) fs v
                    pp = fromIntegral p
                    qq = fromIntegral q
                    newfs = pp*fs/qq
                    t0 = V.head (head inputPart)
                    tr = [t0,t0+1/newfs..]
                mapM_ (\(x,y) -> hPutStrLn stdout $ (show x)++" "++show y)
                  $ zip tr (V.toList (resamplePart (inputPart!!1)))

getPQ x = let a = take 2 $ map (\y-> read y :: Int) $ splitOn "/" x
           in (head a,a!!1)


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
      "Usage: resampler [-t(--withTime)] channel P/Q fs stdin"
  ]
