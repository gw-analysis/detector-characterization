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
  varArgs <- getArgs
  let ch = head varArgs
      (p,q) = getPQ (varArgs !! 1) ::(Int,Int)
      fs = read (varArgs !! 2) :: Double

  let inputPart = unsafePerformIO $ stdin2vec
      resamplePart v = resampleSV (p,q) fs v
  mapM_ (\y -> hPutStrLn stdout $ show y) (V.toList (resamplePart inputPart))


getPQ x = let a = take 2 $ map (\y-> read y :: Int) $ splitOn "/" x
           in (head a,a!!1)



