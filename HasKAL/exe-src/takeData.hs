
import qualified Data.Vector.Storable as V
import HasKAL.IOUtils.Function
import System.Console.GetOpt
import System.Environment (getArgs)
import System.IO (stdout, hPutStrLn)

main = do
  n <- getArgs >>= \args -> case (length args) of
    1 -> return (read (head args) ::Int)
    _ -> error "Usage: takeData n stdin"

  v <- stdin2vec
  let out = V.take n v

  mapM_ (\y -> hPutStrLn stdout $ show y) (V.toList out)
