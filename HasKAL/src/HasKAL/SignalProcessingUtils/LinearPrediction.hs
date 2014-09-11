
module HasKAL.SignalProcessingUtils.LinearPrediction
( lpefCoeff
, levinson
, whitening
) where

import Numeric.LinearAlgebra
import Numeric.GSL.Fourier
import Data.Complex()
import Data.Array
import HasKAL.SignalProcessingUtils.WindowType
import HasKAL.SignalProcessingUtils.WindowFunction
import HasKAL.SignalProcessingUtils.Interpolation
import HasKAL.SignalProcessingUtils.InterpolationType
import HasKAL.SpectrumUtils.SpectrumUtils

-- import Prelude hiding (abs)

{- exposed functions -}
lpefCoeff :: Int -> [Double] -> [Double]
lpefCoeff p psddat = do
  let r = [x | x:+_ <-toList $ ifft $ applyRealtoComplex $ fromList psddat]
  levinson r p


levinson :: [Double] -> Int -> [Double]
levinson r p = do
    let r' = array (0, nlen-1) $ zip [0..nlen-1] [x:+0|x<-r]
        (tmpcoef, rho) = levinson' r' p
    1:(map ((/sqrt rho).realPart) $ elems tmpcoef)
    where nlen = length r


whitening :: Int -> [Double] -> Int -> Double -> [Double] -> ([Double], [Double])
whitening nC trainDat trainNfft samplingFrequency gwdat = do
  let datLen = length gwdat
      trainDatLen = length trainDat
      (trainfV, trainpsd) = unzip $ gwpsd trainDat trainNfft samplingFrequency
      trainpsdInterp = interpV trainfV trainpsd (toList $ linspace datLen (0,  samplingFrequency)) Linear
      whnCoeff = lpefCoeff (nC-1) $ map ((fromIntegral datLen)*) trainpsdInterp

  let fftGwdat = fft.toComplex $ (windowed (hanning datLen) (fromList gwdat),  constant 0 datLen)
      fft_whnCoeff = mapVector magnitude $ fft $ join [fromList [x:+0|x<-whnCoeff], constant (0:+0) (datLen-nC)]
      whnGwfft = toComplex (fft_whnCoeff*(mapVector realPart fftGwdat),  mapVector imagPart fftGwdat)
  (toList $ mapVector realPart (ifft whnGwfft), whnCoeff)


{- internal functions -}
tuplify2 :: Vector Double -> Vector Double -> (Vector Double,  Vector Double)
tuplify2 x y = (y,  x)

applyRealtoComplex :: Vector Double -> Vector (Complex Double)
applyRealtoComplex x = toComplex $ tuplify2 (constant 0 nfft) x
  where nfft = dim x

levinson' :: (Ix a, Integral a, RealFloat b) => Array a (Complex b) -- ^ r
                                             -> a                   -- ^ p
                                             -> (Array a (Complex b),b) -- ^ (a,rho)
levinson' r p = (array (1,p) [ (k, a!(p,k)) | k <- [1..p] ], realPart (rho!p))
  where
    a   = array ((1,1),(p,p)) [ ((k,i), ak k i) | k <- [1..p], i <- [1..k] ]
    rho = array (1,p) [ (k, rhok k) | k <- [1..p] ]
    ak 1 1             = -r!1 / r!0
    ak k i | k==i      = -(r!k + sum [ a!(k-1,l) * r!(k-l) | l <- [1..(k-1)] ]) / rho!(k-1)
           | otherwise = a!(k-1,i) + a!(k,k) * (conjugate (a!(k-1,k-i)))
    rhok 1 = (1 - (abs (a!(1,1)))^(2::Int)) * r!0
    rhok k = (1 - (abs (a!(k, k)))^(2::Int)) * rho!(k-1)





