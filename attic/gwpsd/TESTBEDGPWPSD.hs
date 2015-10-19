

{-# OPTIONS_GHC -XBangPatterns #-}


module TESTBEDGPWPSD
( gwpsdV
, gwspectrogramV
)
where


{- Signature -}
import HasKAL.SpectrumUtils.Signature
import HasKAL.SpectrumUtils.Function

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
import Control.Parallel.Strategies (runEval, parMap, rdeepseq, rseq, rpar, rparWith, withStrategy, parBuffer, parList)

import qualified Control.Monad.Par.Scheds.Trace as Par
import qualified Control.Monad.Par as Par
import qualified Control.Monad.Par.Combinator as Par



gwpsdV :: Vector Double -> Int -> Double -> (Vector Double, Vector Double)
gwpsdV dat nfft fs = gwpsdCoreVP Welch dat nfft fs Hann


gwpsdCoreVP :: PSDMETHOD -> Vector Double -> Int -> Double -> WindowType -> (Vector Double, Vector Double)
gwpsdCoreVP method dat nfft fs w
  | method==Welch = gwOnesidedPSDWelchP3 dat nfft fs w
  | otherwise =  error "No such method implemented. Check GwPsdMethod.hs"



gwpsdCoreVS :: PSDMETHOD -> Vector Double -> Int -> Double -> WindowType -> (Vector Double, Vector Double)
gwpsdCoreVS method dat nfft fs w
  | method==Welch = gwOnesidedPSDWelchS2 dat nfft fs w
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


--slow
gwOnesidedPSDWelchS1 dat nfft fs w = do
  let ndat = dim dat
      maxitr = ndat `div` nfft :: Int
 --     !datlist = mkChunks dat nfft :: [Vector Double]
      datlist = takesV (take maxitr (repeat nfft)) dat
      onesided = map (\v-> scale (2.0/(fromIntegral nfft * fs)) $ calcPower nfft $ (mapVector (**2.0) (dftRH1d . applyWindowFunction w $ v))) datlist
      outs = 1/(fromIntegral maxitr) * foldl1' (+) onesided
   in (linspace (succ $ nfft `div` 2) (0, fs/2), outs)


-- | parallized
gwOnesidedPSDWelchP2' :: Vector Double
                     -> Int
                     -> Double
                     -> WindowType
                     -> (Vector Double, Vector Double)
gwOnesidedPSDWelchP2' dat nfft fs w = do
  let ndat = dim dat
      maxitr = ndat `div` nfft :: Int
      --datlist = takesV (take maxitr (repeat nfft)) dat :: [Vector Double]
      datlist = mkChunks dat nfft :: [Vector Double]
      twosided = withStrategy (parBuffer 2 rdeepseq) $ map (\v-> scale (2.0/(fromIntegral nfft * fs)) (toSquaredSum . fromComplex . dftRC1d . applyWindowFunction w $ v)) datlist
      outs = 1/(fromIntegral maxitr) * foldl1' (+) twosided
   in (linspace (succ $ nfft `div` 2) (0, fs/2), outs)


-- fast
gwOnesidedPSDWelchS2 dat nfft fs w = do
  let datlist = mkChunks dat nfft :: [Vector Double]
      maxitr = length datlist
      psdgain = 2.0/(fromIntegral nfft * fs)
      ffted = mapFFT . mapApplyWindowFunction w $ datlist
      power = map (fst . fromComplex) $ zipWith (*) ffted (map conj ffted)
      outs = scale (psdgain/fromIntegral maxitr) $ foldr1 (+) power
   in (linspace (succ $ nfft `div` 2) (0, fs/2), outs)
   where
     mapFFT = map dftRC1d
     mapApplyWindowFunction windowtype
       | windowtype==Hann = map (windowed (hanning nfft))
       | otherwise = error "No such window implemented. Check WindowType.hs"



-- | second fastest
gwOnesidedPSDWelchP2 dat nfft fs w = do
  let datlist = mkChunks dat nfft :: [Vector Double]
      maxitr = length datlist
      psdgain = 2.0/(fromIntegral nfft * fs)
      ffted = withStrategy (parBuffer 3 rdeepseq) $  parmapFFT . parmapApplyWindowFunction w $ datlist
--      wed = parmapApplyWindowFunction w $ datlist
--      ffted = parmapFFT wed
--      ffted = parmapFFT . parmapApplyWindowFunction w $ datlist
      power = map (fst . fromComplex) $ zipWith (*) ffted (map conj ffted)
      outs = scale (psdgain/(fromIntegral maxitr)) $ foldr1 (+) power
   in (linspace (succ $ nfft `div` 2) (0, fs/2), outs)
   where
     parmapFFT = map dftRC1d
     parmapApplyWindowFunction windowtype
       | windowtype==Hann = map (windowed (hanning nfft))
       | otherwise = error "No such window implemented. Check WindowType.hs"



-- | fastest,  using Par monad
gwOnesidedPSDWelchP3 dat nfft fs w = Par.runPar $ do
  let datlist = mkChunks dat nfft :: [Vector Double]
      maxitr = length datlist
      psdgain = 2.0/(fromIntegral nfft * fs)
  wed <- parmapApplyWindowFunction w $ datlist
  ffted <- parmapFFT wed
  power <- Par.parMap (fst . fromComplex) $ zipWith (*) ffted (map conj ffted)
  let outs = scale (psdgain/fromIntegral maxitr) $ foldr1 (+) power
  return (linspace (succ $ nfft `div` 2) (0, fs/2), outs)
   where
     parmapFFT = Par.parMap dftRC1d
     parmapApplyWindowFunction windowtype
       | windowtype==Hann = Par.parMap (windowed (hanning nfft))
       | otherwise = error "No such window implemented. Check WindowType.hs"




-- | fast
gwOnesidedPSDWelchS3 dat nfft fs w = do
  let ndat = dim dat
      maxitr = ndat `div` nfft :: Int
      --datlist = takesV (take maxitr (repeat nfft)) dat :: [Vector Double]
      datlist = takesV (take maxitr (repeat nfft)) dat
      --datlist = mkChunks dat nfft :: [Vector Double]

      --twosided = map (\v-> scale (2.0/(fromIntegral nfft * fs)) (toSquaredSum . fromComplex . dftRC1d . applyWindowFunction w $ v)) datlist
      ffted = mapFFT . mapApplyWindowFunction w $ datlist
      power = map (abs . fst . fromComplex) $ zipWith (*) ffted (map conj ffted)
      --power =
      outs = 1/(fromIntegral maxitr) * foldr (+) (zeros (nfft`div`2+1)) power
      psdgain = 2.0/(fromIntegral nfft * fs)
   in (linspace (succ $ nfft `div` 2) (0, fs/2),scale psdgain outs)
   where
     mapFFT = map dftRC1d
     mapApplyWindowFunctions windowtype
       | windowtype==Hann = map (windowed (hanning nfft))
       | otherwise = error "No such window implemented. Check WindowType.hs"
     mapApplyWindowFunction w = map (applyWindowFunction2 w nfft)



gwspectrogramV :: Int -> Int -> Double -> Vector Double -> Spectrogram
gwspectrogramV noverlap nfft fs x = (tV, freqV, specgram)
  where freqV = subVector 0 nfft2 $ linspace nfft (0, fs/2)
        tV    = fromList [(fromIntegral nshift)/fs*fromIntegral y | y<-[0..nt]]
        specgram = fromColumns
          $ map (\m -> (snd $ gwpsdV (subVector (m*nshift) nfft x) nfft fs)) [0..nt] :: Matrix Double
        nt =  (dim x -nfft) `div` nshift
        nshift = nfft - noverlap
        nfft2 = succ $ div nfft 2




zeros :: Int -> Vector Double
zeros nzero = constant 0 nzero


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

applyWindowFunction2 :: WindowType -> Int -> Vector Double -> Vector Double
applyWindowFunction2 windowtype nfft
  | windowtype==Hann = windowed (hanning nfft)
  | otherwise = error "No such window implemented. Check WindowType.hs"


mkChunks :: VS.Vector Double -> Int -> [VS.Vector Double]
mkChunks vIn n = mkChunksCore vIn n (VS.length vIn `div` n)
  where
    mkChunksCore _ _ 0 = []
    mkChunksCore vIn n m = VS.slice 0 n vIn :  mkChunksCore (VS.drop n vIn) n (m-1)




