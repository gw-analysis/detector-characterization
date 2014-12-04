



module HasKAL.SpectrumUtils.Function
( plotFormatedSpectogram
, updateMatrixElement
, updateSpectrogramSpec
, lengthTime
, lengthFreq
, getSpectrum
, getTimeEvolution
) where

import Control.Monad.ST (ST)
import Data.Packed.ST
import Numeric.LinearAlgebra
import HasKAL.SpectrumUtils.Signature


plotFormatedSpectogram :: Spectrogram -> [(Double, Double, Double)]
plotFormatedSpectogram (tV, freqV, specgram) = do
  let tV' = concat [ replicate (dim freqV) x | x <- toList tV]
      freqV'=take (dim tV * dim freqV) (cycle $ toList freqV)
  zip3 tV' freqV' (concat $ map (\x->toList x) $ toColumns specgram)


updateMatrixElement :: Matrix Double -> [(Int, Int)] -> [Double] -> Matrix Double
updateMatrixElement s w x = runSTMatrix $ do
  case length w == length x of
    True -> do s' <- unsafeThawMatrix s
               mapM_ (\i->unsafeWriteMatrix s' (fst (w!!i)) (snd (w!!i)) (x!!i)) [0..length w-1]
               return s'
    False -> error "should be same length"

updateSpectrogramSpec :: Spectrogram -> Matrix Double -> Spectrogram
updateSpectrogramSpec s m = (t, f, m)
  where (t, _, _) = s
        (_, f, _) = s

lengthTime :: Spectrogram -> Int
lengthTime (_, _, specgram) = cols specgram

lengthFreq :: Spectrogram -> Int
lengthFreq (_, _, specgram) = rows specgram

getSpectrum :: Int -> Spectrogram -> Spectrum
getSpectrum n (_, freqV, specgram) = do
  let specV = flatten $ takeColumns 1 $ dropColumns n specgram
  (freqV, specV)

getTimeEvolution :: Int -> Spectrogram -> Spectrum
getTimeEvolution n (tV, _, specgram) = do
  let evolV = flatten $ takeRows 1 $ dropRows n specgram
  (tV, evolV)