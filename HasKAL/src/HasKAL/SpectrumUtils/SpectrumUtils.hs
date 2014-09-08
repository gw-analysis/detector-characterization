{-# OPTIONS_GHC -XBangPatterns #-}

module HasKAL.SpectrumUtils.SpectrumUtils
(  gwpsd
 , gwspectrogram
)
where

{- For fft -}
import Numeric.GSL.Fourier
import Numeric.LinearAlgebra

{- psd method type -}
import HasKAL.SpectrumUtils.GwPsdMethod

{- for windowing -}
import HasKAL.SignalProcessingUtils.WindowType
import HasKAL.SignalProcessingUtils.WindowFunction

--import HasKAL.SpectrumUtils.AuxFunction(sort, median)
import Data.List (sort, foldl')


gwspectrogram :: Int -> Int -> Double -> [Double] -> [(Double, Double, Double)]
gwspectrogram noverlap nfft fs x = genTFData tV freqV spec
  where freqV = take (div nfft 2) $ toList $ linspace nfft (0, fs)
        tV    = [(fromIntegral nshift)/fs*fromIntegral y | y<-[1..(nt-1)]]
        spec = map (\m -> (take (div nfft 2)).snd.unzip $ gwpsd (toList $ subVector (m*nshift) nfft (fromList x)) nfft fs) [0..(nt-1)] :: [[Double]]
        nt = floor $ (fromIntegral (length x -nfft)) /(fromIntegral nshift)
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
  let ndat = length dat
      maxitr = floor $ fromIntegral (ndat) / fromIntegral (nfft) :: Int
      datlist = takesV (take maxitr (repeat nfft)) $ fromList dat
      fft_val = applyFFT . applytoComplex . applyTuplify2 . applyWindow w $ datlist
      power =  map (abs . fst . fromComplex) $ zipWith (*) fft_val (map conj fft_val)
      meanpower = scale (1/(fromIntegral maxitr)) $ foldr (+) (zeros nfft) power
      scale_psd = 1/(fromIntegral nfft * fs)
  zip (toList $ linspace nfft (0, fs)) (toList $ scale scale_psd meanpower)
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

gwpsdMedianAverageCore :: [Double] -> Int -> Double -> WindowType -> [(Double, Double)]
gwpsdMedianAverageCore dat nfft fs w = do
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
      fvec = toList $ linspace nfft (0, fs) :: [Double]
  zip fvec medianAverageSpectrum


--psdOdd:: [Double] -> Int -> Double -> WindowType -> [Vector (Double, Double)]
psdOdd:: [Double] -> Int -> WindowType -> [Vector Double]
psdOdd dat nfft w = do
  let ndat = length dat :: Int
      maxitr = floor $ fromIntegral (ndat) / fromIntegral (nfft) :: Int
      datlist = takesV (take maxitr (repeat nfft)) $ fromList dat :: [Vector Double]
--      power = forM datlist $ \x -> calcPower x fs w
--  forM power $ \x -> zipVector (linspace nfft (0, fs)) x
  map (\x -> calcPower x w) datlist

--psdEven:: [Double] -> Int -> Double -> WindowType -> [Vector (Double, Double)]
psdEven:: [Double] -> Int -> WindowType -> [Vector Double]
psdEven dat' nfft w = do
  let dat = drop (div nfft 2) dat'
  psdOdd dat nfft w




{- helper functions -}
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


