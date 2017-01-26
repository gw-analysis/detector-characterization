module HasKAL.MonitorUtils.CorrelationMon.CalCorrelation
       ( correlation
       , takeCorrelation
       , takeCorrelationV
       , alpha2Pvalue
       , significance
       , findIndexMaxCorrelationMaxValueIndex
       , findIndexMaxCorrelationMaxValueIndexV
       , correlationChunk
       , correlationChunkV
       )
       where

import Data.List (transpose)

{-- Vector --}
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Unboxed as U

import HasKAL.MonitorUtils.CorrelationMon.CorrelationMethod
import HasKAL.StatisticsUtils.Correlation.MIC(micU')
import HasKAL.ExternalUtils.GSL.RandomNumberDistributions
import HasKAL.SignalProcessingUtils.Resampling (downsampleSV, downsampleUV)
import HasKAL.WaveUtils.Data(WaveData(..))

{-- ToDo :
    ・CGIだけのためにデータのサンプリングレートを揃えるLPFをかける関数を用意する
    ・CGI用のmodule
    ・異なるサンプリングレートでも動くようにする
    ・引数にfs_xとfs_yを追加する
--}


{-- Expose Functions --}
correlation :: CorrelationMethod -- ^ Pearson / MIC
            -> WaveData          -- ^ data
            -> WaveData          -- ^ data
            -> Double            -- ^ jitter of correlation [s]
            -> [Double]          -- coefficients
correlation method xw yw dt = S.toList $
  takeCorrelationCheckSamplingFrequencyV method fsx fsy x y nshift
 where
  fsx = samplingFrequency xw
  x = gwdata xw
  fsy = samplingFrequency yw
  y = gwdata yw
  nshift | fsx >= fsy = floor (fsy * dt)
         | fsx <  fsy = floor (fsx * dt)


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


takeCorrelationCheckSamplingFrequency :: CorrelationMethod -- ^ Pearson / MIC
                                      -> Double -- ^ sampling rate [Hz] of data x
                                      -> Double -- ^ sampling rate [Hz] of data y
                                      -> [Double] -- ^ data list x
                                      -> [Double] -- ^ data list y
                                      -> Int -- ^ number of shift to take correlation
                                      -> [Double] -- ^ list of correlation coefficient [x_2, x_-1, x_0, x_1, x_2]
takeCorrelationCheckSamplingFrequency method fsx fsy x y nshift = U.toList $ takeCorrelationCheckSamplingFrequencyCore method fsx fsy (U.fromList x) (U.fromList y) nshift

takeCorrelationCheckSamplingFrequencyV :: CorrelationMethod -- ^ Pearson / MIC
                                      -> Double -- ^ sampling rate [Hz] of data x
                                      -> Double -- ^ sampling rate [Hz] of data y
                                      -> S.Vector Double -- ^ data Vector x
                                      -> S.Vector Double -- ^ data Vector y
                                      -> Int -- ^ number of shift to take correlation
                                      -> S.Vector Double -- ^ Vector of correlation coefficient [x_2, x_-1, x_0, x_1, x_2]
takeCorrelationCheckSamplingFrequencyV method fsx fsy x y nshift = U.convert $ takeCorrelationCheckSamplingFrequencyCore method fsx fsy (U.convert x) (U.convert y) nshift


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


correlationChunk :: CorrelationMethod -- ^ Pearson / MIC
                              -> [Double] -- ^ data x
                              -> [Double] -- ^ data y
                              -> Double -- ^ total duration to analyze [s]
                              -> Double -- ^ chunk duration to analyze [s]
                              -> Double -- ^ sampling rate [Hz]
                              -> Int    -- ^ number of shift to take correlation
                              -> ([Double], [Double], [Double]) -- ^ ([time], [rho_max], [timeshift])
correlationChunk method x y ttotal tchunk fs nshift = do
  let result = correlationChunkCore method (U.fromList x) (U.fromList y) ttotal tchunk fs nshift :: (S.Vector Double, S.Vector Double, S.Vector Double)
      tolist :: (S.Vector Double, S.Vector Double, S.Vector Double) -> ([Double], [Double], [Double])
      tolist (ele0, ele1, ele2) = (S.toList ele0, S.toList ele1, S.toList ele2)
  tolist result

correlationChunkV :: CorrelationMethod -- ^ Pearson / MIC
                              -> S.Vector Double -- ^ data x
                              -> S.Vector Double -- ^ data y
                              -> Double -- ^ total duration to analyze [s]
                              -> Double -- ^ chunk duration to analyze [s]
                              -> Double -- ^ sampling rate [Hz]
                              -> Int    -- ^ number of shift to take correlation
                              -> (S.Vector Double, S.Vector Double, S.Vector Double) -- ^ vector of correlation coefficient maximized ([time], [rho_max], [timeshift])
correlationChunkV method x y ttotal tchunk fs nshift = correlationChunkCore method (U.convert x) (U.convert y) ttotal tchunk fs nshift






{-- Internal Functions --}
takeCorrelationCore :: CorrelationMethod -- ^ Pearson / MIC
                    -> U.Vector Double -- ^ data Vector x
                    -> U.Vector Double -- ^ data Vector y
                    -> Int -- ^ number of shift to take correlation
                    -> U.Vector Double -- ^ Vector of correlation coefficient
takeCorrelationCore method x y nshift = case method of
  Pearson -> twoChannelData2CorrelationV x y nshift
  MIC    -> twoChannelData2MICV x y nshift

twoChannelData2CorrelationV :: U.Vector Double -- ^ data Vector x
                            -> U.Vector Double -- ^ data Vector y
                            -> Int -- ^ number of shift to take correlation
                            -> U.Vector Double -- ^ Vector of correlation coefficient
twoChannelData2CorrelationV x y nshift
  | U.length x == 0 = U.fromList []
  | U.length y == 0 = U.fromList []
  | nshift < 0      = U.fromList []
  | otherwise       = U.map (timeshiftedData2CorrelationV x y) $ U.fromList [-nshift..nshift]
  where timeshiftedData2CorrelationV :: U.Vector Double -> U.Vector Double -> Int -> Double
        timeshiftedData2CorrelationV x y n
          | n > dataLength = pearsonCorrelationV (dataHeadDropNV x dataLength) y
          | n < 0          = pearsonCorrelationV (dataHeadDropNV y (-n) ) x
          | otherwise      = pearsonCorrelationV (dataHeadDropNV x n    ) y
          where dataLength = max (U.length x) (U.length y)


twoChannelData2MICV :: U.Vector Double -- ^ data Vector x
                    -> U.Vector Double -- ^ data Vector y
                    -> Int -- ^ number of shift to take correlation
                    -> U.Vector Double -- ^ Vector of correlation coefficient
twoChannelData2MICV x y nshift
  | U.length x == 0 = U.fromList []
  | U.length y == 0 = U.fromList []
  | nshift < 0      = U.fromList []
  | otherwise       = U.map (timeshiftedData2CorrelationV x y) $ U.fromList [-nshift..nshift]
  where timeshiftedData2CorrelationV :: U.Vector Double -> U.Vector Double -> Int -> Double
        timeshiftedData2CorrelationV x y n
          | n > dataLength = micU' (dataHeadDropNV x dataLength) y
          | n < 0          = micU' (dataHeadDropNV y (-n) ) x
          | otherwise      = micU' (dataHeadDropNV x n    ) y
          where dataLength = max (U.length x) (U.length y)


dataHeadDropNV ::  U.Vector Double -- ^ data list
               -> Int -- ^ number of sample to drop
               -> U.Vector Double
dataHeadDropNV listData n = U.drop n listData

--dataTailDropNV ::  U.Vector Double -> Int -> U.Vector Double
--dataTailDropNV listData n = U.take ((U.length listData) - n) listData

takeCorrelationCheckSamplingFrequencyCore :: CorrelationMethod -- ^ Pearson / MIC
                                          -> Double -- ^ sampling rate [Hz] of x
                                          -> Double -- ^ sampling rate [Hz] of y
                                          -> U.Vector Double -- ^ vector x
                                          -> U.Vector Double -- ^ vector y
                                          -> Int -- ^ number of shift to take correlation
                                          -> U.Vector Double -- ^ Vector fo correlation coefficient
takeCorrelationCheckSamplingFrequencyCore method fsx fsy x y nshift
  | fsx == fsx                                 = takeCorrelationCore method x y nshift
  | (truncate $ fsl / fss) == 0 && (fsx > fsy) = takeCorrelationCore method (dsFunction fsx fsy x) y nshift
  | (truncate $ fsl / fss) == 0 && (fsx < fsy) = takeCorrelationCore method x (dsFunction fsy fsx y) nshift
  | otherwise                                  = takeCorrelationCore method x y nshift
     where fsl = max fsx fsy
           fss = min fsx fsy
-- How should the statement(otherwise) be treated? Now otherwise statement does nothing.

dsFunction :: Double -> Double -> U.Vector Double -> U.Vector Double
dsFunction fs newfs xs = dsCore fs p xs
       where dsCore :: Double -> Int -> U.Vector Double -> U.Vector Double
             dsCore fs 0 xs = xs
             dsCore fs n xs = dsCore fs (n-1) $ downsampleUV fs (fs/2) xs
             p = truncate $ fs / newfs



correlationChunkCore :: CorrelationMethod -- ^ Pearson / MIC
                     -> U.Vector Double -- ^ data x
                     -> U.Vector Double -- ^ data y
                     -> Double -- ^ total duration to analyze [s]
                     -> Double -- ^ chunk duration to analyze [s]
                     -> Double -- ^ sampling rate [Hz]
                     -> Int    -- ^ number of shift to take correlation
                     -> (S.Vector Double, S.Vector Double, S.Vector Double) -- ^ vector of correlation coefficient maximized ([time], [rho_max], [timeshift])
correlationChunkCore method x y ttotal tchunk fs nshift = case method of
  Pearson -> correlationChunkPearson x y ttotal tchunk fs nshift
  MIC    -> correlationChunkMIC x y ttotal tchunk fs nshift

correlationChunkPearson :: U.Vector Double -- ^ data x
                        -> U.Vector Double -- ^ data y
                        -> Double -- ^ total duration to analyze [s]
                        -> Double -- ^ chunk duration to analyze [s]
                        -> Double -- ^ sampling rate [Hz]
                        -> Int    -- ^ number of shift to take correlation
                        -> (S.Vector Double, S.Vector Double, S.Vector Double) -- ^ vector of correlation coefficient maximized ([time], [rho_max], [timeshift])
correlationChunkPearson x y ttotal tchunk fs nshift
  | U.length x == 0 = emptyResult
  | U.length y == 0 = emptyResult
  | fs < 0          = emptyResult
  | nshift < 0      = emptyResult
  | otherwise       = execute x y nchunk fs nshift
   where nx     = U.length x :: Int
         ny     = U.length y :: Int
         nxy_min = min nx ny
         ntotal = floor (ttotal * fs) :: Int
         nchunk = floor (tchunk * fs) :: Int
         nchunkset = (min ntotal nxy_min) `div` nchunk
         nchunk_list = [0..(nchunkset-1)] :: [Int]

         emptyResult ::(S.Vector Double, S.Vector Double, S.Vector Double)
         emptyResult = (S.fromList [], S.fromList [], S.fromList [])

         correlationChunkPearsonCore :: U.Vector Double -> U.Vector Double -> Int -> Double -> Int -> Int-> [Double]
         correlationChunkPearsonCore x y nchunk fs nshift index = do
           let rhov   = twoChannelData2CorrelationV (U.slice (nchunk*index) nchunk x) (U.slice (nchunk*index) nchunk y) nshift :: U.Vector Double
               output = formatoutput rhov index nchunk fs
                  where rhomax = findIndexMaxCorrelationMaxValueIndexV (U.convert rhov) fs :: (Int, Double)
                        nchunkd = fromIntegral nchunk :: Double
                        indexd  = fromIntegral index  :: Double
                        formatoutput :: U.Vector Double -> Int -> Int -> Double -> [Double]
                        formatoutput rhov index nchunk fs = [indexd / fs * nchunkd, (rhov U.! (fst rhomax)), snd rhomax]
           output

         execute :: U.Vector Double -> U.Vector Double -> Int -> Double -> Int -> (S.Vector Double, S.Vector Double, S.Vector Double)
         execute x y nchunk fs nshift = do
           let result   = transpose $ map (correlationChunkPearsonCore x y nchunk fs nshift ) nchunk_list :: [[Double]]
               resultv  = map (U.convert . U.fromList) result :: [S.Vector Double]

               resultv_tuple :: [S.Vector Double] -> (S.Vector Double, S.Vector Double, S.Vector Double)
               resultv_tuple resultv = (resultv!!0, resultv!!1, resultv!!2)
           resultv_tuple resultv


correlationChunkMIC :: U.Vector Double -- ^ data x
                    -> U.Vector Double -- ^ data y
                    -> Double -- ^ total duration to analyze [s]
                    -> Double -- ^ chunk duration to analyze [s]
                    -> Double -- ^ sampling rate [Hz]
                    -> Int    -- ^ number of shift to take correlation
                    -> (S.Vector Double, S.Vector Double, S.Vector Double) -- ^ vector of correlation coefficient maximized ([time], [rho_max], [timeshift])
correlationChunkMIC x y ttotal tchunk fs nshift
  | U.length x == 0 = emptyResult
  | U.length y == 0 = emptyResult
  | fs < 0          = emptyResult
  | nshift < 0      = emptyResult
  | otherwise       = execute x y nchunk fs nshift
   where nx     = U.length x :: Int
         ny     = U.length y :: Int
         nxy_min = min nx ny
         ntotal = floor (ttotal * fs) :: Int
         nchunk = floor (tchunk * fs) :: Int
         nchunkset = (min ntotal nxy_min) `div` nchunk
         nchunk_list = [0..(nchunkset-1)] :: [Int]

         emptyResult ::(S.Vector Double, S.Vector Double, S.Vector Double)
         emptyResult = (S.fromList [], S.fromList [], S.fromList [])

         correlationChunkMICCore :: U.Vector Double -> U.Vector Double -> Int -> Double -> Int -> Int-> [Double]
         correlationChunkMICCore x y nchunk fs nshift index = do
           let rhov   = twoChannelData2MICV (U.slice (nchunk*index) nchunk x) (U.slice (nchunk*index) nchunk y) nshift :: U.Vector Double
               output = formatoutput rhov index nchunk fs
                  where rhomax = findIndexMaxCorrelationMaxValueIndexV (U.convert rhov) fs :: (Int, Double)
                        nchunkd = fromIntegral nchunk :: Double
                        indexd  = fromIntegral index  :: Double
                        formatoutput :: U.Vector Double -> Int -> Int -> Double -> [Double]
                        formatoutput rhov index nchunk fs = [indexd / fs * nchunkd, (rhov U.! (fst rhomax)), snd rhomax]
           output

         execute :: U.Vector Double -> U.Vector Double -> Int -> Double -> Int -> (S.Vector Double, S.Vector Double, S.Vector Double)
         execute x y nchunk fs nshift = do
           let result   = transpose $ map (correlationChunkMICCore x y nchunk fs nshift ) nchunk_list :: [[Double]]
               resultv  = map (U.convert . U.fromList) result :: [S.Vector Double]

               resultv_tuple :: [S.Vector Double] -> (S.Vector Double, S.Vector Double, S.Vector Double)
               resultv_tuple resultv = (resultv!!0, resultv!!1, resultv!!2)
           resultv_tuple resultv



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
