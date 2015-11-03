


module HasKAL.MonitorUtils.CoherenceMon.Function (
  hBruco,
  hBrucoSameFs,
  coherenceMon,
  coherenceMonSameFs,
  delayCoherenceMonSameFs,
  coherenceTFMonSameFs
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

hBrucoSameFs :: Int -> Double -> (V.Vector Double, String) -> [(V.Vector Double, String)] -> [(Double, [(Double, String)])]
hBrucoSameFs nfft fs (xt, xch) yts = zip fvec result
  where result = map (reverse.sort.(`zip` (map snd yts))).toLists.fromColumns $ map (snd.(coherenceMonSameFs nfft fs xt).fst) yts
        fvec = [0, fs/(fromIntegral nfft)..]

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
        xt1 = map (\i -> V.slice i nfftx xt) [0, nfftx..(V.length xt)-nfftx]
        yt1 = map (\i -> V.slice i nffty yt) [0, nffty..(V.length yt)-nffty]
        xf1 = map dftRC1d xt1
        yf1 = map dftRC1d yt1
        pxx1 = ave $ map ps xf1
        pyy1 = ave $ map ps yf1
        pxy1 = ave $ zipWith cs xf1 yf1
        coh'f1 = V.slice 0 (V.length fv) $ coh pxy1 pxx1 pyy1

coherenceMonSameFs :: Int              -- ^ length of FFT
                   -> Double           -- ^      sampling: fs
                   -> V.Vector Double  -- ^ time series 1: x(t)
                   -> V.Vector Double  -- ^ time series 2: y(t)
                   -> Spectrum         -- ^     coherency: |coh(f)|^2
coherenceMonSameFs nfft fs xt yt = (fv, coh'f1)
  where nfft' = min nfft $ min (V.length xt) (V.length yt)
        fv = V.fromList [0, fs/(fromIntegral nfft')..fs/2]
        xt1 = map (\i -> V.slice i nfft' xt) [0, nfft'..(V.length xt)-nfft']
        yt1 = map (\i -> V.slice i nfft' yt) [0, nfft'..(V.length yt)-nfft']
        xf1 = map dftRC1d xt1
        yf1 = map dftRC1d yt1
        pxx1 = ave $ map ps xf1
        pyy1 = ave $ map ps yf1
        pxy1 = ave $ zipWith cs xf1 yf1
        coh'f1 = V.slice 0 (V.length fv) $ coh pxy1 pxx1 pyy1

delayCoherenceMonSameFs :: Int -> Double -> V.Vector Double -> V.Vector Double -> Spectrum
delayCoherenceMonSameFs nfft fs xt yt = (fv1, V.zipWith max coh1 coh2)
  where (fv1, coh1) = coherenceMonSameFs nfft fs xt yt
        (fv2, coh2) = coherenceMonSameFs nfft fs (V.drop (nfft`div`2) xt) (V.drop (nfft`div`2) yt)


-- | TF map of coherency
coherenceTFMonSameFs :: Int             -- ^   shift point
               -> Int             -- ^ average point
               -> Int             -- ^ length of FFT
               -> Double          -- ^      sampling: fs
               -> V.Vector Double -- ^ time series 1: x(t)
               -> V.Vector Double -- ^ time series 2: y(t)
               -> Spectrogram     -- ^     coherency: |coh(t, f)|^2|
coherenceTFMonSameFs nshift nchunck nfft fs xt yt = (tV, fV, coh'tf)
  where nchunck' = min nchunck $ min (V.length xt) (V.length yt)
        tV = V.fromList [0, (fromIntegral nshift)/fs..(fromIntegral nstop)/fs]
        fV = V.fromList [0, fs/(fromIntegral nfft)..fs/2]
        coh'tf = M.fromColumns
                 $ map (\i -> snd $ coherenceMonSameFs nfft fs (V.slice i nchunck' xt) (V.slice i nchunck' yt)) [0, nshift..nstop]
        nstop = min (V.length xt) (V.length yt) - nchunck'


-- | power spectrum w/o FFT norm
ps :: V.Vector (Complex Double) -- ^ fourier spectrum: x(f)
   -> V.Vector Double           -- ^   power spectrum: Pxx(f)
ps xf = V.map ((**2).magnitude) xf

-- | cross spectrum w/o FFT norm
cs :: V.Vector (Complex Double) -- ^ fourier spectrum: x(f)
   -> V.Vector (Complex Double) -- ^ fourier spectrum: y(f)
   -> V.Vector (Complex Double) -- ^   cross specturm: Pxy(f)
cs xf yf = V.zipWith (*) xf (V.map conjugate yf)

-- | averaged spectrum
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


