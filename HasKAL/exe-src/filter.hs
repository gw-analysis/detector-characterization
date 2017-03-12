
import Data.Maybe (fromMaybe)
import qualified Data.Vector.Storable as V
import HasKAL.FrameUtils.Function (readFrameV)
import HasKAL.IOUtils.Function
import HasKAL.SignalProcessingUtils.FilterType
import qualified HasKAL.SignalProcessingUtils.ButterWorth as B
import qualified HasKAL.SignalProcessingUtils.Chebyshev as C
import HasKAL.SignalProcessingUtils.FilterX
import System.Console.GetOpt
import System.Environment (getArgs)
import System.IO (stdout, hPutStrLn)
import System.IO.Unsafe (unsafePerformIO)


main = do
  {-- parameters --}
  (ch,fs',fc',ftype') <- getArgs >>= \args -> case length args of
    4 -> return (head args, args!!1, args!!2, args!!3)
    _ -> error "filter channel fs fc ftype stdin"

  let fs = read fs'       :: Double
      fc = read fc'       :: Double
      ftype = read ftype' :: FilterType

  let inputPart = unsafePerformIO $ stdin2vec
  let filterPart v = case ftype of
        Low -> let lpf = C.chebyshev1 6 1 fs fcp Low
                   fcp = 2*fs*tan (2*pi*fc/fs/2)/(2*pi)
                in filtfilt0 lpf v
        High -> let hph = C.chebyshev1 6 1 fs fcp High
                    fcp = 2*fs*tan (2*pi*fc/fs/2)/(2*pi)
                 in filtfilt0 hph v
  mapM_ (\y -> hPutStrLn stdout $ show y) (V.toList (filterPart inputPart))
