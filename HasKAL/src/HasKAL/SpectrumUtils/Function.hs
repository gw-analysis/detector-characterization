



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
, readSpectrum
, writeSpectrum
, readSpectrogram
, writeSpectrogram
) where

import Control.Monad (liftM)
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
  where specgram = trans.(reshape $ dim freqV).fromList $ css
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
normalizeSpectrogram (fv, snf) (tV, fV, hfs) = (tV, fV, newSpecgram)
  where newSpecgram = mapCols1 (\x y -> zipVectorWith (/) y x) snf hfs

normalizeSpectrum :: Spectrum -> Spectrum -> Spectrum
normalizeSpectrum (fv, snf) (fV, hf) = (fV, newSpec)
  where newSpec = zipVectorWith (/) hf snf


-- text IO
readSpectrum :: FilePath -> IO Spectrum
readSpectrum fname = do
  xss <- liftM str2llst $ readFile fname
  case rowsCheck 2 xss of
   True -> return.toSpectrum $ map (\x -> (x!!0, x!!1)) xss
   False -> error $ fname++" is not Spectrum Format"

writeSpectrum :: FilePath -> Spectrum -> IO ()
writeSpectrum fname spec = writeFile fname $ llst2str.map (\(x, y) -> [x, y]).fromSpectrum $ spec
  
readSpectrogram :: FilePath -> IO Spectrogram
readSpectrogram fname = do
  xs3 <- liftM (map str2llst.lines).readFile $ fname :: IO [[[Double]]]
  let xss = concat xs3 -- remove empty list
  case (emptyCheck xs3, rowsCheck 3 xss) of
   (True, True) -> return.toSpectrogram $ map (\x -> (x!!0, x!!1, x!!2)) xss
   (_, _) -> error $ fname++" is not Spectrogram Format"

writeSpectrogram :: FilePath -> Spectrogram -> IO ()
writeSpectrogram fname spec = writeFile fname $ llst2str.insertEmpty.map (\(x, y, z) -> [x, y, z]).plotFormatedSpectogram $ spec
  


{-- Internal Functions --}
emptyCheck :: (Eq a) => [[a]] -> Bool
emptyCheck xss = or $ map (==[]) xss

rowsCheck :: Int -> [[a]] -> Bool
rowsCheck n xss = and $ map ((==n).length) xss

insertEmpty :: [[Double]] -> [[Double]]
insertEmpty (x:y:[]) = [x, y, []]
insertEmpty (x:y:ys) = case (head x == head y) of
 True -> x : insertEmpty (y:ys)
 False -> [x, []] ++ insertEmpty (y:ys)

str2llst :: String -> [[Double]]
str2llst = map (map read.words).lines

llst2str :: [[Double]] -> String
llst2str = unlines.map (unwords.map show)
