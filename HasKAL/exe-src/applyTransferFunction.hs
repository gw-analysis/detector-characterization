
import qualified Data.Vector.Storable as V
import HasKAL.IOUtils.Function
import HasKAL.SignalProcessingUtils.FilterDesign
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
        where header = "Usage: applyTransferFunction [-t(--withTime)] ASDfile fs stdin"

  {-- parameters --}
  (asdfile,fs') <- case (length varArgs) of
    2 -> return (head varArgs, varArgs!!1)
    _ -> error "Usage: applyTransferFunction [-t(--withTime)] ASDfile fs stdin"

  v' <- stdin2vec
  let v = (V.take (floor fs) v') V.++ v' V.++ (V.take (floor fs) v')
      vs = loadASCIIdataCV asdfile
      fs = read fs' :: Double
      fre = head vs
      asd = V.map (*sqrt fs) (vs!!1)
      (b, a) = fir2 3000 (fre, asd)
      filtered = filtfilt0 (b,a) v
      out = V.drop (floor fs) . V.take ((V.length v')+floor fs) $ filtered

  case (optTime varOpt) of
    False -> mapM_ (\y -> hPutStrLn stdout $ show y) (V.toList out)
    True  -> mapM_ (\(x,y) -> hPutStrLn stdout $ (show x)++" "++show y)
                      $ zip [0,1/fs..] (V.toList out)


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
      "Usage: applyTransferFunction [-t(--withTime)] ASDfile fs stdin"
  ]
