
import qualified Data.Vector.Storable as V
import HasKAL.IOUtils.Function
import System.Environment (getArgs)
import System.IO (stdout, hPutStrLn)
import System.IO.Unsafe (unsafePerformIO)


main = do
      let inputPart = unsafePerformIO $ stdin2vec
          maxVal = V.maximum inputPart
          maxInd = V.maxIndex inputPart
      hPutStrLn stdout ((show maxInd)++" "++(show maxVal))
