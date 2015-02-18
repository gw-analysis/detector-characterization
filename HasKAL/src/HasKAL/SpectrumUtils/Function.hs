



module HasKAL.SpectrumUtils.Function
( plotFormatedSpectogram
, toSpectrogram  
, updateMatrixElement
, updateSpectrogramSpec
, lengthTime
, lengthFreq
, getSpectrum
, getTimeEvolution
, toSpectrum
, fromSpectrum
, normalizeSpectrum
, normalizeSpectrogram
) where

import Control.Monad.ST (ST)
import Data.Packed.ST
import Numeric.LinearAlgebra
import HasKAL.SpectrumUtils.Signature
import Data.List (nub)
import Data.Packed.Vector (zipVectorWith)
import HasKAL.Misc.SMatrixMapping (mapCols1)

plotFormatedSpectogram :: Spectrogram -> [(Double, Double, Double)]
plotFormatedSpectogram (tV, freqV, specgram) = do
  let tV' = concat [ replicate (dim freqV) x | x <- toList tV]
      freqV'=take (dim tV * dim freqV) (cycle $ toList freqV)
  zip3 tV' freqV' (concat $ map (\x->toList x) $ toColumns specgram)

toSpectrogram :: [(Double, Double, Double)] -> Spectrogram
toSpectrogram spec = (tV, freqV, specgram)
  where specgram = (reshape $ dim tV).fromList $ css
        freqV = fromList.nub $ bs
        tV = fromList.nub $ as      
        (as, bs, css) = unzip3 spec

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

toSpectrum :: [(Double, Double)] -> Spectrum
toSpectrum spec = (freqV, specV)
  where freqV = fromList as
        specV = fromList bs
        (as, bs) = unzip spec

fromSpectrum :: Spectrum -> [(Double, Double)]
fromSpectrum (freqV, specV) = zip (toList freqV) (toList specV)


-- df of Sn(f) must be the same as one of h(f)
normalizeSpectrogram :: Spectrum -> Spectrogram -> Spectrogram
normalizeSpectrogram snf hoffs = (fst' hoffs, snd' hoffs, newSpecgram)
  where newSpecgram = mapCols1 normalizeSpectrumCore (snd snf) (trd' hoffs)
        fst' (a, _, _) = a
        snd' (_, b, _) = b
        trd' (_, _, c) = c

normalizeSpectrum :: Spectrum -> Spectrum -> Spectrum
normalizeSpectrum snf hoff = (fst hoff, newSpec)
  where newSpec = normalizeSpectrumCore (snd snf) (snd hoff)

normalizeSpectrumCore :: Vector Double -> Vector Double -> Vector Double
normalizeSpectrumCore xv yv = zipVectorWith (/) xv yv
