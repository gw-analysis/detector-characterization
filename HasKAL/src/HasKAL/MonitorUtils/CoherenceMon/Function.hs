


module HasKAL.MonitorUtils.CoherenceMon.Function (
  hBruco,
  coherenceMon,
) where


import Numeric.GSL.Fourier
import qualified Data.Vector.Storable as V
import qualified Data.Packed.Matrix as M
import qualified Data.Vector.Unboxed as U
import Data.Complex
import Data.List (sort, foldl1')
import Data.Matrix.Unboxed (toLists, fromColumns)
import HasKAL.SpectrumUtils.Signature
import HasKAL.MathUtils.FFTW (dftRC1d)


-- | Bruco like tool
hBruco :: Double -> (Double, V.Vector Double, String) -> [(Double, V.Vector Double, String)] -> [(Double, [(Double, String)])]
hBruco sec (fsx, xt, xch) yts = zip fvec result
  where fvec = [0, 1/sec..]
        result = map (ranked.labeled) cohList
          where ranked x = reverse $ sort x
                labeled x = zip x (map trd' yts)
        cohList = toLists.fromColumns $ map (snd.(\x -> coherenceMon sec fsx (fst' x) xt (snd' x))) yts
        fst' (a,_,_) = a
        snd' (_,b,_) = b
        trd' (_,_,c) = c

-- | frequency based coherency
coherenceMon :: Double              -- ^ length of FFT [s]
             -> Double           -- ^ sampling of x(t): fs
             -> Double           -- ^ sampling of y(t): fs
             -> V.Vector Double  -- ^    time series 1: x(t)
             -> V.Vector Double  -- ^    time series 2: y(t)
             -> Spectrum         -- ^     coherency: |coh(f)|^2
coherenceMon sec fsx fsy xt yt = (fv, coh'f1)
  where nfftx = truncate $ fsx * sec
        nffty = truncate $ fsy * sec
        fv = V.fromList [0, fsx/(fromIntegral nfftx)..fsx/2]
        xf = fsd nfftx nfftx xt
        yf = fsd nffty nffty yt
        pxx = apsd xf
        pyy = apsd yf
        pxy = acsd xf yf
        coh'f1 = V.slice 0 (V.length fv) $ coh pxy pxx pyy


{-- Internal Function --}
-- | Fourier spectrum
fsd :: Int                         -- ^ minimum n of y(f) and xi(f)
    -> Int                         -- ^ number of point of FFT
    -> V.Vector Double             -- ^ time series vector
    -> [V.Vector (Complex Double)] -- ^ list of SFT
fsd nmin nfft xt = map ( dftRC1d . (\i -> V.slice i nfft xt) ) [0, nfft..(V.length xt)-nfft]
  where cutHighFreq x = V.slice 0 (nmin `div` 2 + 1) x

-- | Averaged Power spectrum
apsd :: [V.Vector (Complex Double)] -- ^ list of SFT
    -> V.Vector Double             -- ^ power spectrum: S[x, x]
apsd xfs = ave psds
  where psds = map (V.map ( (**2) . magnitude )) xfs

-- | Averaged Cross Spectrum
acsd :: [V.Vector (Complex Double)] -- ^ list of SFT
    -> [V.Vector (Complex Double)] -- ^ list of SFT
    -> V.Vector (Complex Double)   -- ^ cross spectrum: S[x1, x2]
acsd xfs yfs = ave csds
  where csds = zipWith (V.zipWith (*)) xfs (map (V.map conjugate) yfs)

-- | averaged function
ave :: (Fractional a, V.Storable a)
    => [V.Vector a] -- ^  list of spectrum: Pij(f)
    -> V.Vector a   -- ^ averaged spectrum: < Pij(f) >
ave pij = V.map (/ (fromIntegral $ length pij)) sum'pij
  where sum'pij = foldl1' (V.zipWith (+)) pij

-- | coherency between Pxx(f) and Pyy(f)
coh :: V.Vector (Complex Double) -- ^ cross spectrum: Pxy(f)
    -> V.Vector Double           -- ^ power spectrum: Pxx(f)
    -> V.Vector Double           -- ^ power spectrum: Pyy(f) 
    -> V.Vector Double           -- ^       coherent: coh^{2}(f)
coh pxy pxx pyy = V.zipWith (/) coeff denom
  where coeff = V.map ((**2).magnitude) $ pxy
        denom = V.zipWith (*) pxx pyy


