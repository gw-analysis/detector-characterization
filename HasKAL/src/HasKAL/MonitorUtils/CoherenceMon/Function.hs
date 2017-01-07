{-# LANGUAGE FlexibleContexts #-}


module HasKAL.MonitorUtils.CoherenceMon.Function (
  hBrucoW,
  coherenceMonW,
  coherenceMonW',
  coherenceMon,
  multiCoherenceW,
  multiCoherence
) where


import qualified Data.Vector.Storable as V
import qualified Numeric.LinearAlgebra.Data as M
import Numeric.LinearAlgebra as M
import Data.Complex (Complex(..), magnitude, conjugate)
import Data.List (sort, foldl1')
import HasKAL.SpectrumUtils.Signature
import HasKAL.MathUtils.FFTW (dftRC1d)
import Numeric.LinearAlgebra (inv)
import Numeric.LinearAlgebra.HMatrix (eigSH')
import HasKAL.WaveUtils (WaveData(..))

-- | Bruco like tool
hBrucoW :: Double -> (WaveData, String) -> [(WaveData, String)] -> [(Double, [(Double, String)])]
hBrucoW sec (wd1, ch1) wd2 = zip fvec result
  where fvec = [0, 1/sec..]
        result = map (ranked.labeled) cohList
          where ranked x = reverse $ sort x
                labeled x = zip x (map snd wd2)
        cohList = M.toLists.M.fromColumns.map snd $ coherenceMonW' sec wd1 (map fst wd2)

-- | frequency based coherency
coherenceMonW :: Double -> WaveData -> WaveData -> Spectrum
coherenceMonW fftSec w1 w2 = head $ coherenceMon fftSec (samplingFrequency w1) [samplingFrequency w2] (gwdata w1) [gwdata w2]

coherenceMonW' :: Double -> WaveData -> [WaveData] -> [Spectrum]
coherenceMonW' fftSec w1 w2 = coherenceMon fftSec (samplingFrequency w1) (map samplingFrequency w2) (gwdata w1) (map gwdata w2)

coherenceMon :: Double -> Double -> [Double] -> V.Vector Double -> [V.Vector Double] -> [Spectrum]
coherenceMon sec fsx fsys xt yts
  = zipWith (\fsy yt -> coherenceMonCore sec fsx fsy xf pxx yt) fsys yts
  where nfftx = truncate $ fsx * sec
        xf = fsd nfftx nfftx xt
        pxx = apsd xf

multiCoherenceW :: Int -> WaveData -> [WaveData] -> (V.Vector Double, V.Vector Double, [V.Vector (Complex Double)])
multiCoherenceW ny yoft xioft = 
  multiCoherence ny (samplingFrequency yoft) (map samplingFrequency xioft) (gwdata yoft) (map gwdata xioft)

multiCoherence :: Int -> Double -> [Double] -> V.Vector Double -> [V.Vector Double] 
               -> (V.Vector Double, V.Vector Double, [V.Vector (Complex Double)])
multiCoherence ny fsy fsxi yt xit = (fvec, multiCoh vSYY vSYi vDmU, coupleCoeff mSX vSYi)
  where nxi = map (\x -> truncate $ (fromIntegral ny) * x / fsy) fsxi
        nmin = minimum $ ny:nxi
        vY = fsd nmin ny yt
        vXi = zipWith (fsd nmin) nxi xit
        vSYY = apsd vY
        vSYi = M.toRows $ M.fromColumns $ map (acsd vY) vXi
        mSX = csdMat vXi
        vDmU = map eigSH' mSX
        fvec = V.fromList $ map ((*df).fromIntegral) [0,1..nmin`div`2] :: V.Vector Double
        df = fsy / (fromIntegral ny)


{-- Internal Function --}
coherenceMonCore :: Double -- ^ length of FFT [s]
                 -> Double -- ^ sampling of x(t): fs
                 -> Double -- ^ sampling of y(t): fs
                 -> [V.Vector (Complex Double)] -- ^ list of fourier spectrum: X(f)
                 -> V.Vector Double  -- ^ power spectrum: Px(f)
                 -> V.Vector Double  -- ^ time series 2: y(t)
                 -> Spectrum         -- ^ coherency: |coh(f)|^2
coherenceMonCore sec fsx fsy xf pxx yt = (fv, coh'f1)
  where fv = V.fromList [0, 1/sec .. (min fsx fsy)/2]
        nffty = truncate $ fsy * sec
        yf = fsd nffty nffty yt
        pyy = apsd yf
        pxy = acsd xf yf
        coh'f1 = V.slice 0 (V.length fv) $ coh pxy pxx pyy

coupleCoeff :: [M.Matrix (Complex Double)] -> [V.Vector (Complex Double)] -> [V.Vector (Complex Double)]
coupleCoeff mSX vSY = M.toRows $ M.fromColumns $ zipWith prod'MV (map inv mSX) (map (V.map conjugate) vSY)

multiCoh :: V.Vector Double -> [V.Vector (Complex Double)] -> [(V.Vector Double, M.Matrix (Complex Double))] -> V.Vector Double
multiCoh vSYY vSY vDmU = V.map ( \idx -> coh' (vSYY V.! idx) (vSY!!idx) (vDmU!!idx) ) $ V.fromList [0..V.length vSYY - 1]
  where coh' syy vsy (vd, mu) = ( V.sum $ V.zipWith (\x y -> (magnitude x)**2/y) (prod'VM vsy mu) vd ) / syy

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


csdMat :: [[V.Vector (Complex Double)]] -> [M.Matrix (Complex Double)]
csdMat xf = convM $ for [0..len - 1] $ \idxI -> for [0 .. len - 1] $ \idxJ -> acsd (xf!!idxI) (xf!!idxJ)
  where len = length xf
        for xs = (`map` xs)
        convM x = map (f1 x) [0..len'-1]
          where len' = V.length ((x!!0)!!0)
                f1 x i = M.fromLists $ map (map (V.! i)) x

prod'VM :: (Num a, M.Element a) => V.Vector a -> M.Matrix a -> V.Vector a
prod'VM v m
  | l /= r    = error "Length of Vector /= Row of Matrix"
  | otherwise = V.map newElem $ V.fromList [0..c-1]
  where newElem i = V.sum $ V.zipWith (*) v (m @| i)
        (r, c) = (M.rows m, M.cols m)
        l = V.length v
        
prod'MV :: (Num a, M.Element a) => M.Matrix a -> V.Vector a -> V.Vector a
prod'MV m v
  | c /= l    = error "Colmun of Matrix /= Length of Vector"
  | otherwise = V.map newElem $ V.fromList [0..r-1]
  where newElem i = V.sum $ V.zipWith (*) (m @- i) v
        (r, c) = (M.rows m, M.cols m)
        l = V.length v

(@-) :: (Num a, M.Element a) => M.Matrix a -> Int -> V.Vector a
(@-) mat n = (M.flatten . M.takeRows 1 . M.dropRows n) mat

(@|) :: (Num a, M.Element a) => M.Matrix a -> Int -> V.Vector a
(@|) mat n = (M.flatten . M.takeColumns 1 . M.dropColumns n) mat
