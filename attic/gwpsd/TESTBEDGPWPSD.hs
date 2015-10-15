


{-# OPTIONS_GHC -XBangPatterns #-}

module TESTBEDGPWPSD
( gwpsdWelchP
)
where


{- Signature -}
import HasKAL.SpectrumUtils.Signature()
import HasKAL.SpectrumUtils.Function()

{- For fft -}
--import Numeric.GSL.Fourier
import Numeric.LinearAlgebra
import qualified Numeric.LinearAlgebra as NL

{- psd method type -}
import HasKAL.SpectrumUtils.GwPsdMethod()

{- for windowing -}
import HasKAL.SignalProcessingUtils.WindowType
import HasKAL.SignalProcessingUtils.WindowFunction

--import HasKAL.SpectrumUtils.AuxFunction(sort, median)
import Data.List (foldl1')
import Control.Concurrent.Async (async, wait)
import HasKAL.MathUtils.FFTW (dftRH1d)
import System.IO.Unsafe (unsafePerformIO)
import Numeric.LinearAlgebra.Devel (STVector, runSTVector, unsafeThawVector, modifyVector)
import Control.Monad.ST (ST)


import Control.Parallel.Strategies (runEval, parMap, rdeepseq)


-- | parallized
gwpsdWelchP :: Vector Double -> Int -> Double -> WindowType -> (Vector Double, Vector Double)
gwpsdWelchP dat nfft fs w = runEval $ do
  let ndat = dim dat
      maxitr = ndat `div` nfft :: Int
      datlist = takesV (take maxitr (repeat nfft)) dat :: [Vector Double]
      onesided = parMap rdeepseq (\v-> scale (2.0/(fromIntegral nfft * fs)) $ calcPower nfft $ (mapVector (**2.0) (dftRH1d . applyWindowFunction w $ v))) datlist
      outs = 1/(fromIntegral maxitr) * foldl1' (+) onesided
  return $ (linspace (succ $ nfft `div` 2) (0, fs/2), outs)


calcPower :: Int -> Vector Double -> Vector Double
calcPower n v = subVector 0 (n2+1) $ runSTVector $ do
  v' <- unsafeThawVector v
  mapM_ (\i-> calcPowerCore v' v n i) [0..n2]
  return v'
  where n2 = n `div` 2


calcPowerCore :: STVector s Double -> Vector Double -> Int -> Int -> ST s ()
calcPowerCore v' v n i = if i == 0 || i == n2
                           then modifyVector v' i id
                           else modifyVector v' i (+v@>(n-i))
                         where n2 = n `div` 2


applyWindowFunction :: WindowType -> Vector Double -> Vector Double
applyWindowFunction windowtype x
  | windowtype==Hann = windowed (hanning nfft) x
  | otherwise = error "No such window implemented. Check WindowType.hs"
  where nfft = dim x






