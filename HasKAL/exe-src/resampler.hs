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
  {-- parameters --}
  (varOpt, varArgs) <- getArgs >>= \optargs -> 
    case getOpt Permute options optargs of
      (opt, args, []) -> return (Prelude.foldl (flip id) defaultOptions opt, args)
  let ch = head varArgs
      (p,q) = getPQ (varArgs !! 1) ::(Int,Int)
      fs = read (varArgs !! 2) :: Double

  let inputPart = case optInput varOpt of
                    Just "stdin" -> unsafePerformIO $ stdin2vec
                    Just f -> let ch = Prelude.head varArgs
                               in fromMaybe (error "cannot read data.") (unsafePerformIO $ readFrameV ch f)
                    Nothing -> error "cannot read data."
      resamplePart v = resampleSV (p,q) fs v
  mapM_ (\y -> hPutStrLn stdout $ show y) (V.toList (resamplePart inputPart))


getPQ x = let a = take 2 $ map (\y-> read y :: Int) $ splitOn "/" x
           in (head a,a!!1)


data Options = Options
  { optInput    :: Maybe FilePath
  } deriving (Show)


defaultOptions  = Options
  { optInput    = Nothing
  }


options :: [OptDescr (Options -> Options)]
options = 
  [ Option ['i'] ["input"]
      ( OptArg ((\ f opts -> opts {optInput = Just f}) . fromMaybe "stdin") "FILE" )
      "input FILE"
  ]



