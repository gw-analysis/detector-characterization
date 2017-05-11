
import HasKAL.FrameUtils.FileManipulation
import System.Console.GetOpt
import System.Environment (getArgs)
import System.IO (stdout, hPutStrLn, hFlush)


main = do
  (varOpt, varArgs) <- getArgs >>= \optargs ->
    case getOpt Permute options optargs of
      (opt, args,[]) -> return (Prelude.foldl (flip id) defaultOptions opt, args)
      (_  , _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
        where header = "Usage: genFileList [-r(--recursive)] absDir"

  pathlist <- case (length varArgs) of
    1 -> case optRecursive varOpt of
           False -> getFileSystem (head varArgs)
           True  -> getRecursiveFileSystem (head varArgs)
    _ -> error "Usage: genFileList [-r(--recursive)] absDir"

  mapM_ (\x -> hPutStrLn stdout x) pathlist


data Options = Options
  { optRecursive :: Bool
  } deriving (Show)


defaultOptions  = Options
  { optRecursive = False
  }


options :: [OptDescr (Options -> Options)]
options =
  [ Option ['r'] ["recursive"]
      ( NoArg (\ opts -> opts {optRecursive = True}))
      "Usage: genFileList [-r(--recursive)] absDir"
  ]
