
import qualified Data.Vector.Storable as V
import HasKAL.IOUtils.Function
import qualified Numeric.LinearAlgebra as NL
import System.Console.GetOpt
import System.Environment (getArgs)
import System.IO (stdout, hPutStrLn)
import System.IO.Unsafe (unsafePerformIO)


main = do
  (varOpt, varArgs) <- getArgs >>= \optargs ->
    case getOpt Permute options optargs of
      (opt, args,[]) -> return (Prelude.foldl (flip id) defaultOptions opt, args)
      (_  , _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
        where header = "Usage: amplifier [-t(--withTime)] amp stdin"

  {-- parameters --}
  amp' <- case (length varArgs) of
    1 -> return (head varArgs)
    _ -> error "Usage: amplifier [-t(--withTime)] amp stdin"

  let amp = read amp'       :: Double

  case optTime varOpt of
    False -> do
      let inputPart = unsafePerformIO $ stdin2vec
      mapM_ (\y -> hPutStrLn stdout $ show y) (V.toList (NL.scale amp inputPart))
    True  -> do
      let inputPart = unsafePerformIO $ stdin2vecs
      mapM_ (\(x,y) -> hPutStrLn stdout $ (show x)++" "++show y)
               $ zip (V.toList (head inputPart)) (V.toList (NL.scale amp (inputPart!!1)))


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
      "Usage: amplifier [-t(--withTime)] amp stdin"
  ]
