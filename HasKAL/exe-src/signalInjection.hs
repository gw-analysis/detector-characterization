

import qualified Data.Vector.Storable as V
import HasKAL.IOUtils.Function
import HasKAL.SimulationUtils.Injection.Function

import System.Console.GetOpt
import System.Environment (getArgs)
import System.IO (stdout, hPutStrLn)
import System.IO.Unsafe (unsafePerformIO)


main = do
  (varOpt, varArgs) <- getArgs >>= \optargs ->
    case getOpt Permute options optargs of
      (opt, args,[]) -> return (Prelude.foldl (flip id) defaultOptions opt, args)
      (_  , _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
        where header = "Usage: signalInjection [-t(--withTime)] sigfile t0 fs stdin"

  (sigfile, n', t0', fs') <- case length varArgs of
    4 -> return (varArgs!!0, varArgs!!1, varArgs!!2, varArgs!!3)
    _ -> error "Usage: signalInjection [-t(--withTime)] sigfile n t0 fs stdin"


  {-- parameters --}
  let fs = read fs' :: Double   -- seconds
      t0 = read t0' :: Double
      n  = read n' :: Int
  {-- read data --}
  let s1 = head $ loadASCIIdataCV sigfile
      datV  = unsafePerformIO $ stdin2vec
      lenV = V.length datV
      lens1 = V.length s1

  let snend = n + V.length s1
      dn = lenV - snend

  let inj = case dn < 0 of
       True -> do let lens2 = lens1 + dn
                      v2  = V.take lens2 s1
                   in addInjsig n v2 datV
       False-> addInjsig n s1 datV

  case optTime varOpt of
    False -> mapM_ (\y -> hPutStrLn stdout $ show y) (V.toList inj)
    True  -> mapM_ (\(x,y) -> hPutStrLn stdout $ (show x)++" "++show y)
               $ zip [t0,t0+1/fs..] (V.toList inj)





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
