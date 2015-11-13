module HasKAL.MonitorUtils.CorrelationMon.CalCorrelation
       ( takeCorrelation
       , takeCorrelationV
       , alpha2Pvalue
       , significance
       , findIndexMaxCorrelationMaxValueIndex
       , findIndexMaxCorrelationMaxValueIndexV
       )
       where

import HasKAL.MonitorUtils.CorrelationMon.CorrelationMethod
import HasKAL.ExternalUtils.GSL.RandomNumberDistributions

-- for Vector
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Unboxed as U

takeCorrelation :: CorrelationMethod -- ^ Pearson / MIC
                -> [Double] -- ^ data list x
                -> [Double] -- ^ data list y
                -> Int -- ^ number of shift to take correlation
                -> [Double] -- ^ list of correlation coefficient [x_2, x_-1, x_0, x_1, x_2]
takeCorrelation method x y nshift = U.toList $ takeCorrelationCore method (U.fromList x) (U.fromList y) nshift

takeCorrelationV :: CorrelationMethod -- ^ Pearson / MIC
                 -> S.Vector Double -- ^ data Vector x
                 -> S.Vector Double -- ^ data Vector y
                 -> Int -- ^ number of shift to take correlation
                 -> S.Vector Double -- ^ Vector of correlation coefficient 
takeCorrelationV method x y nshift = U.convert $ takeCorrelationCore method (U.convert x) (U.convert y) nshift

takeCorrelationCore :: CorrelationMethod -- ^ Pearson / MIC
                    -> U.Vector Double -- ^ data Vector x
                    -> U.Vector Double -- ^ data Vector y 
                    -> Int -- ^ number of shift to take correlation
                    -> U.Vector Double -- ^ Vector of correlation coefficient 
takeCorrelationCore method x y nshift = case method of
  Peason -> twoChannelData2CorrelationV x y nshift
  MIC    -> twoChannelData2CorrelationV x y nshift

twoChannelData2CorrelationV :: U.Vector Double -- ^ data Vector x
                            -> U.Vector Double -- ^ data Vector y
                            -> Int -- ^ number of shift to take correlation
                            -> U.Vector Double -- ^ Vector of correlation coefficient
twoChannelData2CorrelationV x y nshift
  | U.length x == 0 = U.fromList []
  | U.length y == 0 = U.fromList []
  | nshift < 0        = U.fromList []
  | otherwise       = U.map (timeshiftedData2CorrelationV x y) $ U.fromList [-nshift..nshift]
  where timeshiftedData2CorrelationV :: U.Vector Double -> U.Vector Double -> Int -> Double
        timeshiftedData2CorrelationV x y n
          | n > dataLength = pearsonCorrelationV (dataHeadDropNV x dataLength) y
          | n < 0          = pearsonCorrelationV (dataHeadDropNV y (-n) ) x
          | otherwise      = pearsonCorrelationV (dataHeadDropNV x n    ) y
          where dataLength = max (U.length x) (U.length y)

dataHeadDropNV ::  U.Vector Double -- ^ data list 
               -> Int -- ^ number of sample to drop
               -> U.Vector Double
dataHeadDropNV listData n = U.drop n listData

--dataTailDropNV ::  U.Vector Double -> Int -> U.Vector Double
--dataTailDropNV listData n = U.take ((U.length listData) - n) listData


findIndexMaxCorrelationMaxValueIndex :: [Double] -- ^ list of correlation coefficient
                                     -> Double -- ^ sampling rate [Hz]
                                     -> (Int, Double)
findIndexMaxCorrelationMaxValueIndex result fs = (indexMax, (fromIntegral indexMax) / fs)
  where indexMax = snd $ maximum $ zip result [0, 1..]

findIndexMaxCorrelationMaxValueIndexV :: S.Vector Double -- ^ Vector of correlation coefficient
                                      -> Double -- ^ sampling rate [Hz]
                                      -> (Int, Double)
findIndexMaxCorrelationMaxValueIndexV result fs = (indexMax, (fromIntegral indexMax) / fs)
  where indexMax = S.maxIndex result
      

pearsonCorrelationV :: U.Vector Double -- ^ Vector of data x
                    -> U.Vector Double -- ^ Vector of data y
                    -> Double -- ^ correlation coefficient
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


alpha2Pvalue :: Int -- ^
             -> Double -- ^ 
             -> Double -- ^
alpha2Pvalue n alpha
  | n < 3     = 0
  | otherwise = t / sqrt (realToFrac n - 2.0 + t * t)
  where t = gslCdfTdistPinv (1.0 - alpha/2.0) ( realToFrac (n-2) )


significance :: Int -- ^
             -> Double -- ^
             -> Double -- ^ significance 
significance n r
  | n < 3     = 0
  | abs r > 1 = 0
  | t <  0    = 2.0 * gslCdfTdistP t (realToFrac (n-2))
  | t >= 0    = 2.0 * gslCdfTdistQ t (realToFrac (n-2))
  where t = r * sqrt (realToFrac n - 2) / sqrt (1 - r*r)



