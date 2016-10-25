
import Data.Int (Int32)
import HasKAL.DataBaseUtils.FrameFull.Function (kagraDataGPS)
import HasKAL.FrameUtils.FrameUtils (getChannelList)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.IO (stdout, hPutStrLn)

main = do
  getArgs >>= \args' -> case options args' of
    Right (ss, args) -> case ss of
      [] -> do let method = FromGPS
               go method args
      _  -> do let method = FromFile
               go method args
  where 
    go method args = case (length args) of
        1 -> do let input = head args
                case method of
                  FromGPS -> do
                    kagraDataGPS (read input ::Int32) >>= \maybefiles -> case maybefiles of
                     Nothing -> error "no file found."
                     Just files -> do
                       let fname = head files
                       getChannelList fname >>= \maybech -> case maybech of
                         Nothing -> error "no channel at present."
                         Just x -> mapM_ (\(y, z)-> hPutStrLn stdout $ y++" "++show z) x
                  FromFile -> do
                    getChannelList input >>= \maybech -> case maybech of
                       Nothing -> error "no channel at present."
                       Just x -> mapM_ (\(y, z)-> hPutStrLn stdout $ y++" "++show z) x
        _ -> error "Usage : showChannelandSamplingRate [-f] gps[file]"


data Flag = FromGPS | FromFile deriving Show


options :: [String] -> Either String ([Flag],[String])
options args = case getOpt RequireOrder [Option ['f'] ["file"] (NoArg FromFile) "data from a file"] args of
  (ss, as, []) -> Right (ss, as)
  (_, _, es) -> Left $ concat es


