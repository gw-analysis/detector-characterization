{-# LANGUAGE BangPatterns #-}

module HasKAL.SpectrumUtils.SpectrumUtils
( module HasKAL.SpectrumUtils.Function
, module HasKAL.SpectrumUtils.GwPsdMethod
, module HasKAL.SpectrumUtils.Signature
, gwpsd
, gwspectrogram
, gwpsdV
, gwspectrogramV'
, gwOnesidedPSDV
, gwOnesidedPSDVP
, gwspectrogramV
, gwspectrogramVP1
)
where


import qualified Control.Monad.Par.Scheds.Trace as Par
import qualified Control.Monad.Par as Par
import qualified Control.Monad.Par.Combinator as Par
import Data.List (sort, foldl')
import qualified Data.Vector.Storable as VS
import HasKAL.MathUtils.FFTW (dftRH1d,  dftRC1d)
import HasKAL.SignalProcessingUtils.WindowType
import HasKAL.SignalProcessingUtils.WindowFunction
import HasKAL.SpectrumUtils.Function
import HasKAL.SpectrumUtils.GwPsdMethod
import HasKAL.SpectrumUtils.Signature
import Numeric.GSL.Fourier
import Numeric.LinearAlgebra


{- in case of List data type -}
gwspectrogram :: Int -> Int -> Double -> [Double] -> [(Double, Double, Double)]
gwspectrogram noverlap nfft fs x = genTFData tV freqV spec
  where freqV = take (div nfft 2) $ toList $ linspace nfft (0, fs)
        tV    = [fromIntegral nshift/fs*fromIntegral y | y<-[0..nt]]
        spec = map (\m -> take (div nfft 2).snd.unzip $ gwpsd (toList $ subVector (m*nshift) nfft (fromList x)) nfft fs) [0..nt] :: [[Double]]
        nt =  (length x -nfft)`div`nshift
        nshift = nfft -noverlap


gwpsd :: [Double]-> Int -> Double -> [(Double, Double)]
gwpsd dat nfft fs = gwpsdCore Welch dat nfft fs Hann


gwpsdCore :: PSDMETHOD -> [Double] -> Int -> Double -> WindowType -> [(Double, Double)]
gwpsdCore method dat nfft fs w
  | method==Welch = gwpsdWelch dat nfft fs w
  | method==MedianAverage = gwpsdMedianAverageCore dat nfft fs w
  | otherwise =  error "No such method implemented. Check GwPsdMethod.hs"


gwpsdWelch :: [Double]-> Int -> Double -> WindowType -> [(Double, Double)]
gwpsdWelch dat nfft fs w = do
  let dat' = fromList dat
      (fv, psdv) = gwpsdWelchV dat' nfft fs w
  zip (toList fv) (toList psdv)


gwpsdMedianAverageCore :: [Double] -> Int -> Double -> WindowType -> [(Double, Double)]
gwpsdMedianAverageCore dat nfft fs w = do
  let dat' = fromList dat
      (fv, psdv) = gwpsdMedianAverageCoreV dat' nfft fs w
  zip (toList fv) (toList psdv)


{- in case of Vector data type -}
gwspectrogramV' :: Int -> Int -> Double -> Vector Double -> Spectrogram
gwspectrogramV' noverlap nfft fs x = (tV, freqV, specgram)
  where freqV = subVector 0 nfft2 $ linspace nfft (0, fs)
        tV    = fromList [fromIntegral nshift/fs*fromIntegral y | y<-[0..nt]]
        specgram = fromColumns
          $ map (\m -> (subVector 0 nfft2 (snd $ gwpsdV (subVector (m*nshift) nfft x) nfft fs))) [0..nt] :: Matrix Double
        nt = (dim x -nfft)`div`nshift
        nshift = nfft - noverlap
        nfft2 = div nfft 2


gwpsdV :: Vector Double -> Int -> Double -> (Vector Double, Vector Double)
gwpsdV dat nfft fs = gwpsdCoreV Welch dat nfft fs Hann


gwpsdCoreV :: PSDMETHOD -> Vector Double -> Int -> Double -> WindowType -> (Vector Double, Vector Double)
gwpsdCoreV method dat nfft fs w
  | method==Welch = gwpsdWelchV dat nfft fs w
  | method==MedianAverage = gwpsdMedianAverageCoreV dat nfft fs w
  | otherwise =  error "No such method implemented. Check GwPsdMethod.hs"


gwpsdWelchV :: Vector Double -> Int -> Double -> WindowType -> (Vector Double, Vector Double)
gwpsdWelchV dat nfft fs w = do
  let ndat = dim dat
      maxitr = ndat `div` nfft
      datlist = takesV (replicate maxitr nfft) dat
      fft_val = applyFFT . applytoComplex . applyTuplify2 . applyWindow w $ datlist
      power =  map (abs . fst . fromComplex) $ zipWith (*) fft_val (map conj fft_val)
      meanpower = scale (1/fromIntegral maxitr) $ foldr (+) (zeros nfft) power
      scale_psd = 1/(fromIntegral nfft * fs)
  (linspace nfft (0, fs), scale scale_psd meanpower)
  where
    applyFFT :: [Vector (Complex Double)] -> [Vector (Complex Double)]
    applyFFT = map fft
    applytoComplex :: [(Vector Double, Vector Double)] -> [Vector (Complex Double)]
    applytoComplex = map toComplex
    applyTuplify2 :: [Vector Double] -> [(Vector Double, Vector Double)]
    applyTuplify2 = map (tuplify2 (constant 0 nfft))
    applyWindow :: WindowType -> [Vector Double] -> [Vector Double]
    applyWindow windowtype
      | windowtype==Hann = map (windowed (hanning nfft))
      | otherwise = error "No such window implemented. Check WindowType.hs"


gwpsdMedianAverageCoreV :: Vector Double -> Int -> Double -> WindowType -> (Vector Double, Vector Double)
gwpsdMedianAverageCoreV dat nfft fs w = do
  let --ns = 2*(length dat) `div` nfft - 1 :: Int
      -- number of sample for overwrapping
      --delta = div (length dat) 2 :: Int
      -- calculate median of power spectrum at each frequency bin
      distatFreqOdd = toColumns . fromRows $ (psdOdd dat nfft w) :: [Vector Double]
      nsodd = length distatFreqOdd
      medianListOdd' = map median $ map (sort . toList) distatFreqOdd
      medianListOdd = map (/medianBiasFactor nsodd) medianListOdd'

      distatFreqEven= toColumns . fromRows $ (psdEven dat nfft w) :: [Vector Double]
      nseven = length distatFreqEven
      medianListEven' =  map median $ map (sort . toList) distatFreqEven
      medianListEven= map (/medianBiasFactor nseven) medianListEven'

      medianList = map (/fromIntegral (nsodd+nseven))
        $ zipWith (+) (map (*(fromIntegral nsodd)) medianListOdd) (map (*(fromIntegral nseven)) medianListEven)

      scale_psd = 1/(fromIntegral nfft * fs)
      medianAverageSpectrum = map (*scale_psd) medianList

      -- set corresponding frequency
      fvec = linspace nfft (0, fs) :: Vector Double
  (fvec, fromList medianAverageSpectrum)



gwOnesidedPSDV :: Vector Double -> Int -> Double -> (Vector Double, Vector Double)
gwOnesidedPSDV dat nfft fs = gwOnesidedPSDCoreV Welch dat nfft fs Hann


gwOnesidedPSDCoreV :: PSDMETHOD -> Vector Double -> Int -> Double -> WindowType -> (Vector Double, Vector Double)
gwOnesidedPSDCoreV method dat nfft fs w
  | method==Welch = gwOnesidedPSDWelch dat nfft fs w
  | otherwise =  error "No such method implemented. Check GwPsdMethod.hs"


gwOnesidedPSDWelch :: Vector Double -> Int -> Double -> WindowType -> (Vector Double, Vector Double)
gwOnesidedPSDWelch dat nfft fs w =
  let datlist = mkChunks dat nfft :: [Vector Double]
      maxitr = length datlist
      psdgain = 2.0/(fromIntegral nfft * fs)
      ffted = mapFFT . mapApplyWindowFunction w $ datlist
      power = map (fst . fromComplex) $ zipWith (*) ffted (map conj ffted)
      outs = scale (psdgain/fromIntegral maxitr) $ sum power
   in (fromList [fs*fromIntegral i/fromIntegral nfft|i<-[0..nfft`div`2]], outs)
   where
     mapFFT = map dftRC1d
     mapApplyWindowFunction windowtype
       | windowtype==Hann = map (windowed (hanning nfft))
       | otherwise = error "No such window implemented. Check WindowType.hs"


gwOnesidedPSDVP :: Vector Double -> Int -> Double -> (Vector Double, Vector Double)
gwOnesidedPSDVP dat nfft fs = gwOnesidedPSDCoreVP Welch dat nfft fs Hann


gwOnesidedPSDCoreVP :: PSDMETHOD -> Vector Double -> Int -> Double -> WindowType -> (Vector Double, Vector Double)
gwOnesidedPSDCoreVP method dat nfft fs w
  | method==Welch = gwOnesidedPSDWelchP dat nfft fs w
  | otherwise =  error "No such method implemented. Check GwPsdMethod.hs"


gwOnesidedPSDWelchP dat nfft fs w = Par.runPar $ do
  let datlist = mkChunks dat nfft :: [Vector Double]
      maxitr = length datlist
      psdgain = 2.0/(fromIntegral nfft * fs)
  wed <- parmapApplyWindowFunction w datlist
  ffted <- parmapFFT wed
  power <- Par.parMap (fst . fromComplex) $ zipWith (*) ffted (map conj ffted)
  let outs = scale (psdgain/fromIntegral maxitr) $ sum power
  return (fromList [fs*fromIntegral i/fromIntegral nfft|i<-[0..nfft`div`2]], outs)
   where
     parmapFFT = Par.parMap dftRC1d
     parmapApplyWindowFunction windowtype
       | windowtype==Hann = Par.parMap (windowed (hanning nfft))
       | otherwise = error "No such window implemented. Check WindowType.hs"


gwspectrogramV :: Int -> Int -> Double -> Vector Double -> Spectrogram
gwspectrogramV noverlap nfft fs x = (tV, freqV, specgram)
  where freqV = fromList [fs*fromIntegral i/fromIntegral nfft|i<-[0..nfft`div`2]]
        tV    = fromList [fromIntegral nshift/fs*fromIntegral y | y<-[0..nt]]
        specgram = fromColumns
          $ map (\m -> (snd $ gwOnesidedPSDV (subVector (m*nshift) nfft x) nfft fs)) [0..nt] :: Matrix Double
        nt =  (dim x -nfft) `div` nshift
        nshift = nfft - noverlap



gwspectrogramVP1 :: Int -> Int -> Double -> Vector Double -> Spectrogram
gwspectrogramVP1 noverlap nfft fs x = (tV, freqV, specgram)
  where freqV = fromList [fs*fromIntegral i/fromIntegral nfft|i<-[0..nfft`div`2]]
        tV    = fromList [fromIntegral nshift/fs*fromIntegral y | y<-[0..nt]]
        specgram = fromColumns
          $ map (\m -> (snd $ gwOnesidedPSDVP (subVector (m*nshift) nfft x) nfft fs)) [0..nt] :: Matrix Double
        nt =  (dim x -nfft) `div` nshift
        nshift = nfft - noverlap


{- helper functions -}
mkChunks :: VS.Vector Double -> Int -> [VS.Vector Double]
mkChunks vIn n = mkChunksCore vIn n (VS.length vIn `div` n)
  where
    mkChunksCore _ _ 0 = []
    mkChunksCore vIn n m = VS.slice 0 n vIn :  mkChunksCore (VS.drop n vIn) n (m-1)


--psdOdd:: [Double] -> Int -> Double -> WindowType -> [Vector (Double, Double)]
psdOdd:: Vector Double -> Int -> WindowType -> [Vector Double]
psdOdd dat nfft w = do
  let ndat = dim dat :: Int
      maxitr =  ndat `div` nfft
      datlist = takesV (replicate maxitr nfft) dat :: [Vector Double]
--      power = forM datlist $ \x -> calcPower x fs w
--  forM power $ \x -> zipVector (linspace nfft (0, fs)) x
  map (`calcPower` w) datlist


--psdEven:: [Double] -> Int -> Double -> WindowType -> [Vector (Double, Double)]
psdEven:: Vector Double -> Int -> WindowType -> [Vector Double]
psdEven dat' nfft w = do
  let dat = subVector nfft2 (dim dat' - nfft2) dat'
      nfft2 = div nfft 2
  psdOdd dat nfft w


calcPower :: Vector Double -> WindowType -> Vector Double
calcPower dat w = abs . fst . fromComplex $ fftVal * conj fftVal
  where
    fftVal = fft . applyRealtoComplex . applyWindowFunction w $ dat :: Vector (Complex Double)

applyRealtoComplex :: Vector Double -> Vector (Complex Double)
applyRealtoComplex x = toComplex $ tuplify2 (constant 0 nfft) x
  where nfft = dim x

applyWindowFunction :: WindowType -> Vector Double -> Vector Double
applyWindowFunction windowtype x
  | windowtype==Hann = windowed (hanning nfft) x
  | otherwise = error "No such window implemented. Check WindowType.hs"
  where nfft = dim x

zeros :: Int -> Vector Double
zeros nzero = constant 0 nzero

tuplify2 :: Vector Double -> Vector Double -> (Vector Double, Vector Double)
tuplify2 x y = (y, x)


mean :: Floating a => [a] -> a
mean x = fst $ foldl' (\(!m, !n) x -> (m+(x-m)/(n+1),n+1)) (0,0) x

median :: (Floating a, Ord a) => [a] -> a
median x | odd n  = head  $ drop (n `div` 2) x'
         | even n = mean $ take 2 $ drop i x'
                  where i = (length x' `div` 2) - 1
                        x' = sort x
                        n  = length x

medianBiasFactor :: Int -> Double
medianBiasFactor n = foldl' (\acc x -> acc + (-1)**(fromIntegral x+1)/fromIntegral x) 0 [1..n]



genTFData :: [Double] -> [Double] -> [[Double]] -> [(Double,   Double,   Double)]
genTFData tV freqV spec = do
  let tV' = concat [ replicate (length freqV) x | x <- tV]
      freqV'=take (length tV * length freqV) $ cycle freqV
  zip3 tV' freqV' (concat spec)


