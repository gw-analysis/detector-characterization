

module HasKAL.SignalProcessingUtils.Resampling
( downsample
, downsampleV
, upsample
, resample
, downsampleWaveData
) where

import HasKAL.SignalProcessingUtils.Filter
import HasKAL.SignalProcessingUtils.FilterType
import HasKAL.SignalProcessingUtils.ButterWorth
import HasKAL.TimeUtils.Function
import HasKAL.WaveUtils.Data
import Numeric.LinearAlgebra

downsample :: Double -> Double -> [Double] -> [Double]
downsample fs newfs x = y
  where y = snd.unzip $ filter (\(n, _) -> n `mod` p==1) $ zip [1..] x'
        p = truncate (fs/newfs)
        x' = toList $ iir lpf $ fromList x
        lpf = butter 2 fs (newfs/2) Low


upsample :: Double -> Double -> [Double] -> [Double]
upsample fs newfs x = undefined


resample :: Double -> Double -> [Double] -> [Double]
resample fs newfs x = undefined


downsampleWaveData :: Double -> WaveData -> WaveData
downsampleWaveData newfs x = y
  where y = x
        samplingFrequency y = newfs
        gwdata y = fromList $ snd.unzip $ filter (\(n, _) -> n `mod` p==1) $ zip [1..] (toList gwx')
        p = truncate (samplingFrequency x/newfs)
        gwx' = iir lpf (gwdata x)
        lpf = butter 2 newfs (newfs/2) Low
        stopGPSTime y = formatGPS $ deformatGPS (startGPSTime x) + 1/newfs*fromIntegral (dim (gwdata x))


downsampleV :: Double -> Double -> Vector Double -> Vector Double
downsampleV fs newfs x = y
  where y = fromList $ snd.unzip $ filter (\(n, _) -> n `mod` p == 1) $ zip [1..] $ toList x'
        p = truncate (fs/newfs)
        x' = iir lpf x
        lpf = butter 2 fs (newfs/2) Low






