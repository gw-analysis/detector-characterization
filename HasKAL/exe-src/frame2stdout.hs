
import qualified Data.Vector.Storable as V
import HasKAL.FrameUtils.Function (readFrameV)
import HasKAL.IOUtils.Function
import System.Environment (getArgs)
import System.IO (stdout, hPutStrLn)
import System.IO.Unsafe (unsafePerformIO)

main = do
  (ch,fname) <- getArgs >>= \args -> case (length args) of
    2 -> return (head args, args !!1)
    _ -> error "Usaage: frame2stdout ch framefile"

  readFrameV ch fname >>= \maybex ->
    case maybex of
      Nothing -> error "not valid data."
      Just v -> mapM_ (\x -> hPutStrLn stdout $ show x) xs
                  where xs = V.toList v
