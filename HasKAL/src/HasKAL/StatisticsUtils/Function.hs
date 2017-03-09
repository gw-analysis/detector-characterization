{-# LANGUAGE BangPatterns #-}

module HasKAL.StatisticsUtils.Function
( calculatePeasonCorrelation
, permutationTestPeasonCorrelation
, mean
, histogram1d
)
where


import Foreign.C.Types
-- import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Array
import System.IO.Unsafe
import Data.List

{- exposed functions -}
calculatePeasonCorrelation :: [Double] -> [Double] -> Double
calculatePeasonCorrelation data1 data2 = do
  let data1' = map realToFrac data1
      data2' = map realToFrac data2
      len'   = fromIntegral $ length data1'
  realToFrac $ calculatePeasonCorrelationCore data1' data2' len'

permutationTestPeasonCorrelation :: [Double] -> [Double] -> Int -> Double
permutationTestPeasonCorrelation data1 data2 repeatTimes = do
  let data1' = map realToFrac data1
      data2' = map realToFrac data2
      len'   = fromIntegral $ length data1'
      repeatTimes' = fromIntegral repeatTimes :: CInt
  realToFrac $ permutationTestPeasonCorrelationCore data1' data2' len' repeatTimes'

-- |Numerically stable mean
-- from hstats
mean :: Floating a => [a] -> a
mean x = fst $ foldl' (\(!m, !n) x -> (m+(x-m)/(n+1), n+1)) (0, 0) x


histogram1d :: Double -> Double -> [Double] -> [Double] -> ([Double], [Double])
histogram1d xmin xmax bins input =
  let intervals = zipWith (\x y ->(x, y)) (init bins) (tail bins)
      within u x = x >= fst u && x < snd u
   in (map fst intervals, map ((fromIntegral.length) . (\u -> filter (within u) input)) intervals)



{- internal functions -}

calculatePeasonCorrelationCore :: [CDouble] -> [CDouble] -> CInt -> CDouble
calculatePeasonCorrelationCore data1 data2 len
  = unsafePerformIO $ withArray data1 $ \ptr'data1 ->
    withArray data2 $ \ptr'data2 ->
    c'calculatePeasonCorrelation ptr'data1 ptr'data2 len

permutationTestPeasonCorrelationCore :: [CDouble] -> [CDouble] -> CInt -> CInt -> CDouble
permutationTestPeasonCorrelationCore data1 data2 len repeatTimes
  = unsafePerformIO $ withArray data1 $ \ptr'data1 ->
    withArray data2 $ \ptr'data2 ->
    c'permutationTestPeasonCorrelation ptr'data1 ptr'data2 len repeatTimes


{- import functions -}

foreign import ccall "statisticsUtils.h calculatePeasonCorrelation" c'calculatePeasonCorrelation :: Ptr CDouble -> Ptr CDouble -> CInt -> IO CDouble
foreign import ccall "statisticsUtils.h permutationTestPeasonCorrelation" c'permutationTestPeasonCorrelation :: Ptr CDouble -> Ptr CDouble -> CInt -> CInt -> IO CDouble
