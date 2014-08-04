

module HasKAL.StatisticsUtils.Functions
( calculatePeasonCorrelation
, permutationTestPeasonCorrelation
, calculatePeasonCorrelationCore
, permutationTestPeasonCorrelationCore
--,
)
where


import Foreign.C.Types
-- import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Array
import System.IO.Unsafe


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



