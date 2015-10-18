

{-# OPTIONS_GHC -XBangPatterns #-}


module TESTBEDGPWPSD
--( gwpsdV
--, gwOnesidedPSDWelchP1
--, gwOnesidedPSDWelchP2
--)
where


{- Signature -}
import HasKAL.SpectrumUtils.Signature()
import HasKAL.SpectrumUtils.Function()

{- For fft -}
--import Numeric.GSL.Fourier
import Numeric.LinearAlgebra
--import qualified Numeric.LinearAlgebra as NL
import qualified Data.Vector.Storable as VS

{- psd method type -}
import HasKAL.SpectrumUtils.GwPsdMethod

{- for windowing -}
import HasKAL.SignalProcessingUtils.WindowType
import HasKAL.SignalProcessingUtils.WindowFunction

--import HasKAL.SpectrumUtils.AuxFunction(sort, median)
import Data.List (foldl1')
--import Control.Concurrent.Async (async, wait)
import HasKAL.MathUtils.FFTW (dftRH1d, dftRC1d)
--import System.IO.Unsafe (unsafePerformIO)
import Numeric.LinearAlgebra.Devel (STVector, runSTVector, unsafeThawVector, modifyVector)
import Control.Monad.ST (ST)

-- paralell
import Control.Parallel.Strategies (runEval, parMap, rdeepseq, rseq, rpar, rparWith)




gwpsdV :: Vector Double -> Int -> Double -> (Vector Double, Vector Double)
gwpsdV dat nfft fs = gwpsdCoreVP Welch dat nfft fs Hann


gwpsdCoreVP :: PSDMETHOD -> Vector Double -> Int -> Double -> WindowType -> (Vector Double, Vector Double)
gwpsdCoreVP method dat nfft fs w
  | method==Welch = gwOnesidedPSDWelchP1 dat nfft fs w
  | otherwise =  error "No such method implemented. Check GwPsdMethod.hs"


-- | parallized
gwOnesidedPSDWelchP1 :: Vector Double
                     -> Int
                     -> Double
                     -> WindowType
                     -> (Vector Double, Vector Double)
gwOnesidedPSDWelchP1 dat nfft fs w = do
  let ndat = dim dat
      maxitr = ndat `div` nfft :: Int
      datlist = mkChunks dat nfft :: [Vector Double]
      onesided = parMap (rparWith rdeepseq) (\v-> scale (2.0/(fromIntegral nfft * fs)) $ calcPower nfft $ (mapVector (**2.0) (dftRH1d . applyWindowFunction w $ v))) datlist
      outs = 1/(fromIntegral maxitr) * foldl1' (+) onesided
   in (linspace (succ $ nfft `div` 2) (0, fs/2), outs)


-- | parallized
gwOnesidedPSDWelchP2 :: Vector Double
                     -> Int
                     -> Double
                     -> WindowType
                     -> (Vector Double, Vector Double)
gwOnesidedPSDWelchP2 dat nfft fs w = do
  let ndat = dim dat
      maxitr = ndat `div` nfft :: Int
      datlist = takesV (take maxitr (repeat nfft)) dat :: [Vector Double]
      twosided = parMap rdeepseq (\v-> scale (2.0/(fromIntegral nfft * fs)) (toSquaredSum . fromComplex . dftRC1d . applyWindowFunction w $ v)) datlist
      outs = 1/(fromIntegral maxitr) * foldl1' (+) twosided
   in (linspace (succ $ nfft `div` 2) (0, fs/2), outs)


toSquaredSum :: (Vector Double, Vector Double) -> Vector Double
toSquaredSum (v, w) = mapVector (**2.0) v + mapVector (**2.0) w


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


mkChunks :: VS.Vector Double -> Int -> [VS.Vector Double]
mkChunks vIn n = mkChunksCore vIn n (VS.length vIn `div` n)
  where
    mkChunksCore _ _ 0 = []
    mkChunksCore vIn n m = VS.slice 0 n vIn :  mkChunksCore (VS.drop n vIn) n (m-1)




