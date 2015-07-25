
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Unboxed as U
import Numeric.GSL.Fourier (fft)

import qualified Data.Array.Repa as R
--import qualified Data.Array.Repa.Algorithms.Complex as RC
import qualified Data.Array.Repa.Repr.Unboxed as RU
--mport qualified Data.Array.Repa.Algorithms.FFT as RF
import Data.Array.Repa.Eval
import Data.Array.Repa.Repr.ForeignPtr
import qualified Data.Array.Repa.FFTW as RFW
import Data.Complex
import System.IO.Unsafe
import Control.Monad

import Control.DeepSeq (deepseq)
import Data.Time

main = do
  -- 基本情報
  let fs = 16384.0
      nfft = truncate fs
      tt = [0, 1/fs..64-1/fs]

  ------------------------------------------------------------------------------------
  -- Stroable データ生成
  let xt = S.map r2c $ S.fromList tt
  xt `deepseq` return ()

  -- FFT (hmatrix) の時間計測
  t1 <- getCurrentTime
  let xf = fft xt
  xf `deepseq` return ()
  t2 <- getCurrentTime
  print $ diffUTCTime t2 t1

  ------------------------------------------------------------------------------------
  -- Unboxed データ生成
--  let yt = U.map r2c' $ U.fromList tt
--  yt `deepseq` return ()

  -- FFT (repa) の時間計測(Vector -> Arrayの変換含む)
--  t3 <- getCurrentTime
--  let yf = fft' yt
--  yf `deepseq` return ()
--  t4 <- getCurrentTime
--  print $ diffUTCTime t4 t3

  ------------------------------------------------------------------------------------
  -- Array データ生成
--  let zt = RU.fromUnboxed (R.ix1 $ length tt) $ U.map r2c' $ U.fromList tt
--  zt :: Array F R.DIM1 (Complex Double)
  let zt = fromList (R.ix1 $ length tt) [i :+ 0 | i <- tt] :: Array F R.DIM1 (Complex Double)
  zt `R.deepSeqArray` return ()

  -- FFT (repa) の時間計測
  t5 <- getCurrentTime
  let zf = fftCore zt
  zf `R.deepSeqArray` return ()
  t6 <- getCurrentTime
  print $ diffUTCTime t6 t5

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
--fft' :: U.Vector RC.Complex -> U.Vector RC.Complex
--fft' xt = RU.toUnboxed $ fftCore $ RU.fromUnboxed (R.ix1 $ U.length xt) xt

--fftCore xt = unsafePerformIO $ RF.fft1dP RF.Forward xt

fftCore = RFW.fft

r2c :: Double -> Complex Double
r2c x = x :+ 0

--r2c' :: Double -> RC.Complex
--r2c' x = (x, 0)
