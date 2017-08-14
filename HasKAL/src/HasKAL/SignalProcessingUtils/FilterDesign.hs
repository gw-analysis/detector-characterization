
module HasKAL.SignalProcessingUtils.FilterDesign
  ( fir2
--  ,
  ) where


import qualified Data.Vector.Storable as V
import qualified Numeric.LinearAlgebra as NL
import HasKAL.MathUtils.FFTW
import HasKAL.SignalProcessingUtils.Filter
import HasKAL.SignalProcessingUtils.WindowFunction
import HasKAL.SpectrumUtils.Signature


fir2 :: Int
     -> Spectrum
     -> ([Double], [Double])
fir2 m (fre, spe) =
  let n = m + 1
      npt' | n<1024    = 512 :: Int
           | otherwise = 2^(ceiling (log (fromIntegral n :: Double)/log (2)) :: Int) :: Int
      wind = hamming n
      lap = fix (fromIntegral npt'/25) :: Int
      nbrk = V.length fre
      fre2 = V.toList $ V.map (1/V.last fre *) fre
      aa = V.toList spe
      nint = nbrk - 1
      df = zipWith (-) (tail fre2) (init fre2)
      npt = npt' + 1
      out = Prelude.replicate npt' 0 :: [Double]
      h = calcH out df fre2 aa 1 lap npt nint
      dt = 0.5 * fromIntegral (n-1) :: Double
      rad' = map (\x->(-dt)*pi/(fromIntegral npt-1)*x) [0..fromIntegral (npt-1)] :: [Double]
      rad_real = V.fromList $ map cos rad'
      rad_imag = V.fromList $ map sin rad'
      h_real = V.zipWith (*) (V.fromList h) rad_real
      h_imag = V.zipWith (*) (V.fromList h) rad_imag
      hc = NL.toComplex (h_real, h_imag)
      ht = dftCR1d hc
      b' = V.take n ht

   in (V.toList $ V.zipWith (*) b' wind, (1:replicate (n-1) 0))


calcH :: [Double]
      -> [Double]
      -> [Double]
      -> [Double]
      -> Int
      -> Int
      -> Int
      -> Int
      -> [Double]
calcH _ _ _ _ _ _ _ (-1) = []
calcH [] _ _ _ _ _ _ _ = []
calcH _ [] _ _ _ _ _ _ = []
calcH _ _ [] _ _ _ _ _ = []
calcH _ _ _ [] _ _ _ _ = []
calcH out (df1:df) (_:ff) (aa1:aa) nb' lap npt n =
  let (nb, ne) = case df1==0 of
        True -> let newnb = ceiling ((fromIntegral nb' :: Double) - (fromIntegral lap :: Double)/2)
                 in (newnb, newnb + lap)
        False-> (nb', fix ((head ff)*fromIntegral npt))
      j = [fromIntegral nb..fromIntegral ne] :: [Double]
      inc | nb==ne = [0] :: [Double]
          | otherwise = map (\x->(x-fromIntegral nb)/(fromIntegral ne - fromIntegral nb)) j :: [Double]
      updates = (map (\x->((head aa)-aa1)*x+aa1) inc)
      newout = (take (nb-1) out) ++  updates ++ (drop ne out)
  in case (n-1) of
       0 -> newout
       _ -> calcH newout df ff aa (ne+1) lap npt (n-1)


fix :: Double
    -> Int
fix x | x>=0 = floor x
      | otherwise = ceiling x
