module HasKAL.MonitorUtils.CorrelationMon.CalCorrelation
       ( takeCorrelation
       , takeCorrelationV
       , alpha2Pvalue
       , data2Significance
       , correlationResult2pickupMaxValueIndex
       , correlationResult2pickupMaxValueIndexV
       )
       where

import HasKAL.MonitorUtils.CorrelationMon.CorrelationMethod
import HasKAL.ExternalUtils.GSL.RandomNumberDistributions

-- for Vector
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Unboxed as U

takeCorrelation :: CorrelationMethod -> [Double] -> [Double] -> Int -> [Double]
takeCorrelation method x y maxN = U.toList $ takeCorrelationCore method (U.fromList x) (U.fromList y) maxN

takeCorrelationV :: CorrelationMethod -> S.Vector Double -> S.Vector Double -> Int -> S.Vector Double
takeCorrelationV method x y maxN = U.convert $ takeCorrelationCore method (U.convert x) (U.convert y) maxN

takeCorrelationCore :: CorrelationMethod -> U.Vector Double -> U.Vector Double -> Int -> U.Vector Double
takeCorrelationCore method x y maxN = case method of
  Peason -> twoChannelData2CorrelationV x y maxN
  MIC    -> twoChannelData2CorrelationV x y maxN

twoChannelData2CorrelationV :: U.Vector Double -> U.Vector Double -> Int -> U.Vector Double
twoChannelData2CorrelationV x y maxN
  | U.length x == 0 = U.fromList []
  | U.length y == 0 = U.fromList []
  | maxN < 0        = U.fromList []
  | otherwise       = U.map (timeshiftedData2CorrelationV x y) $ U.fromList [0..maxN]
  where timeshiftedData2CorrelationV :: U.Vector Double -> U.Vector Double -> Int -> Double
        timeshiftedData2CorrelationV listX listY intN
          | intN < dataLength = pearsonCorrelationV (dataHeadDropNV listX intN) listY
          | otherwise         = pearsonCorrelationV (dataHeadDropNV listX dataLength) listY
          where dataLength = max (U.length listX) (U.length listY)

dataHeadDropNV ::  U.Vector Double -> Int -> U.Vector Double
dataHeadDropNV listData intN = U.drop intN listData

dataTailDropNV ::  U.Vector Double -> Int -> U.Vector Double
dataTailDropNV listData intN = U.take ((U.length listData) - intN) listData


-- index of max correlation = 
-- takeCorrelationの返り値は(a, int, a)にしたいが、
-- MICの返り値も同じ形になるとは限らないので、独立した別の関数にするか？
-- let result = twoChannelData2Correlation modData1 modData2 100
-- let indexMaxCorrelation = snd $ maximum $ zip result [1..]
correlationResult2pickupMaxValueIndex :: [Double] -> Double -> (Int, Double)
correlationResult2pickupMaxValueIndex result srate = (indexMax, (fromIntegral indexMax) / srate)
  where indexMax = snd $ maximum $ zip result [0, 1..]

correlationResult2pickupMaxValueIndexV :: S.Vector Double -> Double -> (Int, Double)
correlationResult2pickupMaxValueIndexV result srate = (indexMax, (fromIntegral indexMax) / srate)
  where indexMax = S.maxIndex result
      
--(indexMax, indexMax * srate)

    --  where indexMax = maximum [1..10]


pearsonCorrelationV :: U.Vector Double -> U.Vector Double -> Double
pearsonCorrelationV x y = sxy / (sqrt sxx) / (sqrt syy)
  where n = min (U.length x) (U.length y)
        (x', y') = (U.take n x, U.take n y)
        xmean = (U.sum x') / fromIntegral n
        ymean = (U.sum y') / fromIntegral n
        xdiff = U.map (+(-xmean)) x'
        ydiff = U.map (+(-ymean)) y'
        sxy = U.sum $ U.zipWith (*) xdiff ydiff
        sxx = U.sum $ U.map (**2) xdiff
        syy = U.sum $ U.map (**2) ydiff


alpha2Pvalue :: Int -> Double -> Double
alpha2Pvalue n alpha
  | n < 3     = 0
  | otherwise = t / sqrt (realToFrac n - 2.0 + t * t)
  where t = gslCdfTdistPinv (1.0 - alpha/2.0) ( realToFrac (n-2) )


data2Significance :: Int -> Double -> Double
data2Significance n r
  | n < 3     = 0
  | abs r > 1 = 0
  | t <  0    = 2.0 * gslCdfTdistP t (realToFrac (n-2))
  | t >= 0    = 2.0 * gslCdfTdistQ t (realToFrac (n-2))
  where t = r * sqrt (realToFrac n - 2) / sqrt (1 - r*r)



