
import Data.List (isInfixOf)
import qualified Data.Vector.Storable as V
import HasKAL.IOUtils.Function
import HasKAL.SignalProcessingUtils.WindowFunction
import System.Console.GetOpt
import System.Environment (getArgs)
import System.IO (stdout, hPutStrLn)
import System.IO.Unsafe (unsafePerformIO)


main = do
  (varOpt, varArgs) <- getArgs >>= \optargs ->
    case getOpt Permute options optargs of
      (opt, args,[]) -> return (Prelude.foldl (flip id) defaultOptions opt, args)
      (_  , _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
        where header = "Usage: window [-t(--withTime), -w(--windowtype) WinType] stdin"

  {-- parameters --}
  let v = unsafePerformIO $ stdin2vecs
      n = V.length (head v)


  let windowPart = case optWin varOpt of
       [] -> windowed (hanning n)
       w  | isInfixOf "Hann" w     -> windowed (hanning n)
          | isInfixOf "Blackman" w -> windowed (blackman n)
          | isInfixOf "Hamming" w  -> windowed (hamming n)
          | isInfixOf "Tukey" w    -> do let p = read (drop 5 w) :: Double
                                          in windowed (tukeywin p n)
          | isInfixOf "Kaiser" w   -> do let p = read (drop 5 w) :: Double
                                          in windowed (kaiser p n)
          | otherwise -> error "window function not corrected"

  case optTime varOpt of
    False -> do
      let inputPart = head v
      mapM_ (\y -> hPutStrLn stdout $ show y) (V.toList (windowPart inputPart))
    True  -> do
      let inputPart = v
      mapM_ (\(x,y) -> hPutStrLn stdout $ (show x)++" "++show y)
               $ zip (V.toList (head inputPart)) (V.toList (windowPart (inputPart!!1)))



data Options = Options
  { optTime :: Bool
  , optWin  :: String
  } deriving (Show)


defaultOptions  = Options
  { optTime = False
  , optWin  = "Hann"
  }


options :: [OptDescr (Options -> Options)]
options =
  [ Option ['t'] ["withTime"]
      ( NoArg (\ opts -> opts {optTime = True}))
      "Usage: window [-t(--withTime), -w(--windowtype) WinType] stdin"
  , Option ['w'] ["windowtype"]
      ( ReqArg (\w opts -> opts {optWin = w}) "window type")
      "window type"
  ]
