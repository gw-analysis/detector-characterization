
module HasKAL.WaveUtils.Function (
  catWaveData,
  catWaveData0,
  mergeWaveData0,
  waveData2TimeSeries,
  mergeOverlapWaveDataC,
  getMaximumChunck,
) where

import Debug.Trace (trace)
import Data.List (foldl1')
import Data.Vector.Storable (Vector, fromList)
import qualified Data.Vector.Storable as V (length, concat, replicate, drop, map, maxIndex)

import HasKAL.TimeUtils (GPSTIME, deformatGPS, formatGPS, diffGPS, addGPS)
import HasKAL.WaveUtils.Data (WaveData(..), mkWaveData, dropWaveData)


catWaveData :: [WaveData] -> WaveData
catWaveData ws = mkWaveData (detector w) (dataType w) (samplingFrequency w) (startGPSTime w) endGPS (gwdata w)
  where w = foldl1' (mergeWaveDataCore False) ws
        endGPS = addGPS (startGPSTime w) . formatGPS . (/(samplingFrequency w)) . fromIntegral . V.length $ gwdata w

catWaveData0 :: [WaveData] -> WaveData
catWaveData0 = foldl1' mergeWaveData0

mergeWaveData0 :: WaveData -> WaveData -> WaveData
mergeWaveData0 w1 w2 = mergeWaveDataCore True w1 w2 

waveData2TimeSeries :: GPSTIME -> WaveData -> (Vector Double, Vector Double)
waveData2TimeSeries stdGPS w = (tv, gwdata w)
  where offset = floor . (*(samplingFrequency w)) . deformatGPS $ diffGPS (startGPSTime w) stdGPS
        tv = V.map ( (/(samplingFrequency w)) . fromIntegral . (+offset) ) $ fromList [0 .. V.length (gwdata w) - 1]


mergeOverlapWaveDataC :: [WaveData] -> [WaveData]
mergeOverlapWaveDataC (w1:[])     = [w1]
mergeOverlapWaveDataC (w1:w2:[])
  | chkOverlap w1 w2 == True  = [mergeWaveData0 w1 w2]
  | chkOverlap w1 w2 == False = [w1, w2]
mergeOverlapWaveDataC (w1:w2:ws)
  | chkOverlap w1 w2 == True  = mergeOverlapWaveDataC $ (mergeWaveData0 w1 w2):ws
  | chkOverlap w1 w2 == False = w1 : mergeOverlapWaveDataC (w2:ws)


getMaximumChunck :: [WaveData] -> WaveData
getMaximumChunck ws = ws!!maxIdx
  where maxIdx = V.maxIndex $ fromList $ map (V.length . gwdata) ws


{-- Internal Function --}
mergeWaveDataCore :: Bool -> WaveData -> WaveData -> WaveData
mergeWaveDataCore flg w1 w2
  | chkWD w1 w2 == False = w1
  | chkWD w1 w2 == True = mkWaveData (detector w1) (dataType w1) fs1 (startGPSTime w1) (stopGPSTime w2) dat
  where fs1 = samplingFrequency w1
        nOverlap = floor . (*fs1) . deformatGPS $ diffGPS (stopGPSTime w1) (startGPSTime w2)
        dat = case flg of
               True -> V.concat [gwdata w1, V.replicate (negate nOverlap) 0.0, V.drop nOverlap $ gwdata w2]
               False -> V.concat [gwdata w1, V.drop nOverlap $ gwdata w2]

chkWD :: WaveData -> WaveData -> Bool
chkWD w1 w2 = and [(detector w1) == (detector w2)
                  ,(dataType w1) == (dataType w2)
                  ,(samplingFrequency w1) == (samplingFrequency w2)
                  ,(startGPSTime w1) <= (startGPSTime w2)
                  ,(stopGPSTime w1) <= (stopGPSTime w2)
                  ]

chkOverlap :: WaveData -> WaveData -> Bool
chkOverlap w1 w2 = and [(stopGPSTime w1) > (startGPSTime w2), chkWD w1 w2]

