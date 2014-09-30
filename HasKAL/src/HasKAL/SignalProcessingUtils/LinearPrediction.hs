
module HasKAL.SignalProcessingUtils.LinearPrediction
( lpefCoeff
, levinson
, whitening
, whiteningWaveData
) where

import Data.Maybe
import Numeric.LinearAlgebra
import Numeric.GSL.Fourier
import Data.Complex()
import qualified Data.Array.Unboxed as UV
import HasKAL.SignalProcessingUtils.FilterH
import HasKAL.SignalProcessingUtils.WindowType
import HasKAL.SignalProcessingUtils.WindowFunction
import HasKAL.SignalProcessingUtils.Interpolation
import HasKAL.SignalProcessingUtils.InterpolationType
import HasKAL.SpectrumUtils.SpectrumUtils
import HasKAL.WaveUtils.Data
 

{- exposed functions -}
lpefCoeff :: Int -> [(Double,Double)] -> ([Double],Double)
lpefCoeff p psddat = (out,rho)
  where
    (out,rho) = levinson p r
    r = toList.fst.fromComplex.ifft.fromList 
      $ [fs*nn*x/nn:+0|x<-(snd.unzip) psddat]
    fs = last.fst.unzip $ psddat
    nn = fromIntegral $ length psddat :: Double


levinson :: Int -> [Double] -> ([Double],Double)
levinson p r = do
    let r' = UV.listArray (0, nlen-1) r
        (tmpcoef, rho) = levinsonD r' p
    (1:UV.elems tmpcoef,rho)
    where nlen = length r


whitening :: ([Double],Double) -> [Double]-> [Double]
whitening (whnb,rho) x = map (/sqrt rho) $ fir whnb x


--whiteningC :: ([Double],Double) -> [Double]-> [Double]
--whiteningC (whnb,rho) x = map (/sqrt rho) $ firFilter x whnb


whiteningWaveData :: ([Double],Double) -> WaveData -> WaveData
whiteningWaveData (whnb,rho) x = do
  let y = map (/sqrt rho) $ fir whnb (gwdata (toList x))
  fromJust $ updateWaveDatagwdata x (fromList y)


{- internal functions -}
levinsonD :: (UV.Ix a, Integral a, RealFloat b) 
  => UV.Array Int Double           -- ^ r
  -> Int                           -- ^ p
  -> (UV.Array Int Double, Double) -- ^ (coefficients,rho)
levinsonD r p = (UV.array (1,p) [ (k, a UV.!(p,k)) | k <- [1..p] ], rho UV.!p)
  where
    a = UV.array ((1,1),(p,p)) 
	  [((k,i), ak k i) | k<-[1..p], i<-[1..k]]:: UV.Array (Int,Int) Double
    rho = UV.array (1,p) [ (k, rhok k) | k <- [1..p] ]:: UV.Array Int Double
    ak 1 1         = -r UV.!1 / r UV.!0
    ak k i | k==i  = -(r UV.!k+sum [a UV.!(k-1,l)*r UV.!(k-l)|l<-[1..(k-1)]])
	                 / rho UV.!(k-1)
           | otherwise = a UV.!(k-1,i) + a UV.!(k,k) * a UV.!(k-1,k-i)
    rhok 1 = (1 - (abs (a UV.!(1,1)))^(2::Int)) * r UV.!0
    rhok k = (1 - (abs (a UV.!(k, k)))^(2::Int)) * rho UV.!(k-1)


