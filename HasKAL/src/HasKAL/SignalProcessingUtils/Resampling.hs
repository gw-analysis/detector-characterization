

module HasKAL.SignalProcessingUtils.Resampling
( downsample
, upsample
, resample
, downsampleWaveData
) where

import HasKAL.SignalProcessingUtils.FilterH
import HasKAL.SignalProcessingUtils.FilterType
import HasKAL.SignalProcessingUtils.ButterWorth
import HasKAL.TimeUtils.Function
import HasKAL.WaveUtils.Data
import Numeric.LinearAlgebra

downsample :: Double -> Double -> [Double] -> [Double]
downsample fs newfs x = y
  where y = snd.unzip $ filter (\(n, _) -> n `mod` p==1) $ zip [1..] x'
        p = truncate (fs/newfs)
        x' = iir_df2 lpf x
        lpf = butter 2 fs (newfs/2) Low


upsample :: Double -> Double -> [Double] -> [Double]
upsample fs newfs x = undefined


resample :: Double -> Double -> [Double] -> [Double]
resample fs newfs x = undefined


downsampleWaveData :: Double -> WaveData -> WaveData
downsampleWaveData newfs x = y
  where y = x
        samplingFrequency y = newfs
        gwdata y = fromList $ snd.unzip $ filter (\(n, _) -> n `mod` p==1) $ zip [1..] gwx'
        p = truncate ((samplingFrequency x)/newfs)
        gwx' = iir_df2 lpf $ toList (gwdata x)
        lpf = butter 2 newfs (newfs/2) Low
        stopGPSTime y = formatGPS $ deformatGPS (startGPSTime x) + 1/newfs*fromIntegral (dim (gwdata x))



