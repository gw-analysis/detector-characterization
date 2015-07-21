{- |
Module      : Function
Description : This is documentation tests.
Copyright   : (c) WhoAmI, 2014
License     : ???
Maintainer  : Takahiro Yamamoto %mail%
Stability   : test
Portability : POSIX

-}{-
  * Last Modified: 2015/07/19 13:26:44
-}


module HasKAL.MonitorUtils.CoherenceMon.Function (
  hBruco,
  coherenceMon
) where


import Numeric.GSL.Fourier
import qualified Data.Vector.Storable as V
import Data.Complex
import Data.List (sort)
import Data.Matrix.Unboxed (toLists, fromColumns)
import HasKAL.SpectrumUtils.Signature

-- | Bruco like tool
hBruco :: Int -> Double -> (V.Vector Double, String) -> [(V.Vector Double, String)] -> [(Double, [(Double, String)])]
hBruco nfft fs (xt, xch) yts = zip fvec result
  where result = map (reverse.sort.(`zip` (map snd yts))).toLists.fromColumns $ map (snd.(coherenceMon nfft fs xt).fst) yts
        fvec = [0, fs/(fromIntegral nfft)..]

-- | frequency based coherency
coherenceMon :: Int              -- ^ length of FFT
          -> Double           -- ^      sampling: fs
          -> V.Vector Double  -- ^ time series 1: x(t)
          -> V.Vector Double  -- ^ time series 2: y(t)
          -> Spectrum         -- ^     coherency: |coh(f)|^2
coherenceMon nfft fs xt yt = (fv, coh'f)
  where xt' = [V.slice i (nfft-1) xt | i <- [0, nfft..(V.length xt)-nfft]]
        yt' = [V.slice i (nfft-1) yt | i <- [0, nfft..(V.length yt)-nfft]]
        pxx = ave $ map ps xt'
        pyy = ave $ map ps yt'
        pxy = ave $ zipWith cs xt' yt'
        fv = V.fromList [0, fs/(fromIntegral nfft)..fs/2]
        coh'f = V.slice 0 (V.length fv) $ coh pxy pxx pyy

-- | power spectrum w/o FFT norm
ps :: V.Vector Double -- ^    time series: x(t)
   -> V.Vector Double -- ^ power spectrum: Pxx(f)
ps xt = V.map ((**2).magnitude) $ fft $ V.map r2c xt

-- | cross spectrum w/o FFT norm
cs :: V.Vector Double           -- ^    time series: x(t)
   -> V.Vector Double           -- ^    time series: y(t)
   -> V.Vector (Complex Double) -- ^ cross specturm: Pxy(f)
cs xt yt = V.zipWith (*) xf (V.map conjugate yf)
  where xf = fft $ V.map r2c xt
        yf = fft $ V.map r2c yt

-- | averaged spectrum
ave :: (Fractional a, V.Storable a)
    => [V.Vector a] -- ^  list of spectrum: Pij(f)
    -> V.Vector a   -- ^ averaged spectrum: < Pij(f) >
ave pij = V.map (/ (fromIntegral $ length pij)) sum'pij
  where sum'pij = foldl1 (V.zipWith (+)) pij

-- | coherency between Pxx(f) and Pyy(f)
coh :: V.Vector (Complex Double) -- ^ cross spectrum: Pxy(f)
    -> V.Vector Double           -- ^ power spectrum: Pxx(f)
    -> V.Vector Double           -- ^ power spectrum: Pyy(f) 
    -> V.Vector Double           -- ^       coherent: coh^{2}(f)
coh pxy pxx pyy = V.zipWith (/) coeff denom
  where coeff = V.map ((**2).magnitude) $ pxy
        denom = V.zipWith (*) pxx pyy

r2c :: Double -> Complex Double
r2c x = x :+ 0
