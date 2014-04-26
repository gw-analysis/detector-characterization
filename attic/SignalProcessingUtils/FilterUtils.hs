{-# LANGUAGE ForeignFunctionInterface #-}

module FIlterUtils
(
    whiteningFilterFreqDomain
--  , whiteningFilterTimeDomain
) where

import Numeric.LinearAlgebra
import Numeric.GSL.Fourier
import Foreign.Ptr(Ptr)
import Foreign.C.Types (CFloat)
import Foreign.Marshal.Array (withArrayLen, peekArray)
--import Foreign.Storable (peek)



whiteningFilterFreqDomain :: Vector Double -> Double -> Vector Double -> Vector Double
whiteningFilterFreqDomain psd fs dat = applyWhitening psd fs nfft . ffted $ dat
  where
    nfft :: Int
    nfft = length $ toList dat

    ffted :: Vector Double -> Vector (Complex Double)
    ffted x = fft . toComplex . flipedtuplified . applyWindow nfft $ x

    flipedtuplified :: Vector Double -> (Vector Double, Vector Double)
    flipedtuplified = flipedtuplify2 (zeros nfft)

    applyWindow :: Int -> Vector Double -> Vector Double
    applyWindow n = windowed (hanning n)

    applyWhitening :: Vector Double -> Double -> Int -> Vector (Complex Double) -> Vector Double
    applyWhitening twosided_psd sampling_freq ndat fftedDat =
      fst . fromComplex $ ifft $ toComplex (whitenedRealPart,  whitenedImagPart)
      where
         whitenedRealPart =
          divide (fst $ fromComplex fftedDat) (sqrt $ scale (fromIntegral ndat * sampling_freq) twosided_psd)
         whitenedImagPart = snd $ fromComplex fftedDat;

zeros :: Int -> Vector Double
zeros = constant 0

flipedtuplify2 :: Vector Double -> Vector Double -> (Vector Double, Vector Double)
flipedtuplify2 x y = (y, x)

windowed :: Vector Double -> Vector Double -> Vector Double
windowed w x = w * x

hanning :: Int -> Vector Double
hanning = makeWindow hanning'

makeWindow :: (Double -> Double -> Double) -> Int -> Vector Double
makeWindow win m =
    let md = fromIntegral m
    in fromList $ map (win md . fromIntegral) [(0::Int)..(m-1::Int)]

hanning' :: Double -> Double -> Double
hanning' m n = 0.5 - 0.5 * cos(2 * pi * n / m)




--whiteningFilterTimeDomain :: [Double] -> [Double] -> IO() --IO [Double]
--whiteningFilterTimeDomain psd input = do
--print 1


levinson :: [Double] -> Int -> IO [Double]
levinson r p = do
  let rv = (p+1) |> r :: Vector Double
      partialrv = subVector 1 p rv
      autCorr = toList $ join [reverseVCD partialrv, rv]
      autCorrFloat = map realToFrac autCorr :: [CFloat]
      rvFloat = map realToFrac (toList rv) :: [CFloat]
      out = replicate (p+1) 0

  ptr_autCorrFloat <- withArrayLen autCorrFloat $ \len ptr_tmp -> do
    return ptr_tmp
  ptr_rvFloat <- withArrayLen rvFloat $ \len ptr_tmp ->
    return ptr_tmp
  ptr_outFloat <- withArrayLen out $ \len ptr_tmp -> do
    return ptr_tmp

  c_nr_toeplz ptr_autCorrFloat ptr_outFloat ptr_rvFloat (p+1)
  outFloat <- peekArray (p+1) ptr_outFloat

  let retval =  map realToFrac outFloat :: [Double]
  return retval


--applyConj :: Vector (Complex Double ) -> Vector (Complex)
--applyConj = mapVector conj

reverseVCD :: Vector Double -> Vector Double
reverseVCD = fromList . reverse . toList



foreign import ccall unsafe "nr.h toeplz" c_nr_toeplz :: Ptr CFloat -> Ptr CFloat ->  Ptr CFloat -> Int -> IO()



