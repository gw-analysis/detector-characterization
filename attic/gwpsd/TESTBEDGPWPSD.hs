


{-# OPTIONS_GHC -XBangPatterns #-}

module TESTBEDGPWPSD
( gwpsdWelchVC
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


import Control.Concurrent.Async (async,  wait)
import HasKAL.MathUtils.FFTW (dftRH1d)
import System.IO.Unsafe (unsafePerformIO)
--main = do
gwpsdWelchVC :: Vector Double -> Int -> Double -> WindowType -> (Vector Double, Vector Double)
gwpsdWelchVC dat nfft fs w = unsafePerformIO $ do
  let ndat = dim dat
      maxitr = ndat `div` nfft :: Int
      datlist = takesV (take maxitr (repeat nfft)) dat :: [Vector Double]
  fftjobs <- mapM (\v->async (return $ scale (2.0/(fromIntegral nfft * fs)) (NL.mapVector (**2.0) (dftRH1d . applyWindowFunction w $ v)))) datlist
  outs <- mapM wait fftjobs :: IO ([Vector Double])
  return $ (linspace nfft (0, fs/2), 1/(fromIntegral maxitr) * foldl1' (+) outs)



applyWindowFunction :: WindowType -> Vector Double -> Vector Double
applyWindowFunction windowtype x
  | windowtype==Hann = windowed (hanning nfft) x
  | otherwise = error "No such window implemented. Check WindowType.hs"
  where nfft = dim x






