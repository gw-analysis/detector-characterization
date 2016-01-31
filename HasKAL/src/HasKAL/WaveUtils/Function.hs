
module HasKAL.WaveUtils.Function (
  catWaveData,
  catWaveData0,
  mergeWaveData0,
  waveData2TimeSeries,
  mergeOverlapWaveDataC,
  getMaximumChunck,
  getCoincidentData,
  getCoincidentTime
) where

import Debug.Trace (trace)
import Data.List (foldl1', sort)
import Data.Maybe (mapMaybe)
import Data.Vector.Storable (Vector, fromList)
import qualified Data.Vector.Storable as V (length, concat, replicate, drop, map, maxIndex)

import HasKAL.TimeUtils (GPSTIME, deformatGPS, formatGPS, diffGPS, addGPS)
import HasKAL.WaveUtils.Data (WaveData(..), mkWaveData, dropWaveData, takeWaveData)


catWaveData :: [WaveData] -> WaveData
catWaveData ws = mkWaveData (detector w) (dataType w) (samplingFrequency w) (startGPSTime w) endGPS (gwdata w)
  where w = foldl1' (mergeWaveDataCore False 0) ws
        endGPS = addGPS (startGPSTime w) . formatGPS . (/(samplingFrequency w)) . fromIntegral . V.length $ gwdata w

catWaveData0 :: Double -> [WaveData] -> WaveData
catWaveData0 x ws = foldl1' (mergeWaveData0 x) ws

mergeWaveData0 :: Double -> WaveData -> WaveData -> WaveData
mergeWaveData0 x w1 w2 = mergeWaveDataCore True x w1 w2 

waveData2TimeSeries :: GPSTIME -> WaveData -> (Vector Double, Vector Double)
waveData2TimeSeries stdGPS w = (tv, gwdata w)
  where offset = floor . (*(samplingFrequency w)) . deformatGPS $ diffGPS (startGPSTime w) stdGPS
        tv = V.map ( (/(samplingFrequency w)) . fromIntegral . (+offset) ) $ fromList [0 .. V.length (gwdata w) - 1]


mergeOverlapWaveDataC :: [WaveData] -> [WaveData]
mergeOverlapWaveDataC (w1:[])     = [w1]
mergeOverlapWaveDataC (w1:w2:[])
  | chkOverlap w1 w2 == True  = [mergeWaveData0 0 w1 w2]
  | chkOverlap w1 w2 == False = [w1, w2]
mergeOverlapWaveDataC (w1:w2:ws)
  | chkOverlap w1 w2 == True  = mergeOverlapWaveDataC $ (mergeWaveData0 0 w1 w2):ws
  | chkOverlap w1 w2 == False = w1 : mergeOverlapWaveDataC (w2:ws)


getMaximumChunck :: [WaveData] -> WaveData
getMaximumChunck ws = ws!!maxIdx
  where maxIdx = V.maxIndex $ fromList $ map (V.length . gwdata) ws

getCoincidentData :: [[WaveData]] -> [[WaveData]]
getCoincidentData ws = map (\x -> mapMaybe (f3 x) tl) $ ws
  where tl = f2 . f1 $ map mergeOverlapWaveDataC ws

getCoincidentTime :: [[(GPSTIME, GPSTIME)]] -> [(GPSTIME, GPSTIME)]
getCoincidentTime = f2 . f0

{-- Internal Function --}
mergeWaveDataCore :: Bool -> Double -> WaveData -> WaveData -> WaveData
mergeWaveDataCore flg x w1 w2
  | chkWD w1 w2 == False = w1
  | chkWD w1 w2 == True = mkWaveData (detector w1) (dataType w1) fs1 (startGPSTime w1) (stopGPSTime w2) dat
  where fs1 = samplingFrequency w1
        nOverlap = floor . (*fs1) . deformatGPS $ diffGPS (stopGPSTime w1) (startGPSTime w2)
        dat = case flg of
               True -> V.concat [gwdata w1, V.replicate (negate nOverlap) x, V.drop nOverlap $ gwdata w2]
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

-- for getCoincidentData
f3 :: [WaveData] -> (GPSTIME, GPSTIME) -> Maybe WaveData
f3 zs (s,e)
  | length ws == 0 = Nothing
  | length ws == 1 = Just (dropWaveData nHead $ takeWaveData (len-nTail) w)
  where ws = filter (\x -> s >= startGPSTime x && e <= stopGPSTime x) $ zs
        w = head ws
        len = V.length $ gwdata w
        fs = samplingFrequency w
        nTail = floor . (*fs) . deformatGPS $ diffGPS (stopGPSTime w) e
        nHead = floor . (*fs) . deformatGPS $ diffGPS s (startGPSTime w)

f2 :: [(GPSTIME, Int)] -> [(GPSTIME, GPSTIME)]
f2 [] = []
f2 xs = g1 (replicate (maximum $ map snd xs) False) xs
  where g1 as []     = []
        g1 as (x:[]) = []
        g1 as (x:y:ys)
          | and bs==True  = (fst x, fst y) : g1 bs (y:ys)
          | and bs==False =                  g1 bs (y:ys)
          where bs = take (n-1) as ++ [not (as!!(n-1))] ++ drop n as
                n = snd x

f1 :: [[WaveData]] -> [(GPSTIME, Int)]
f1 wss = sort . concat $ g1 1 wss
  where g1 n [] = []
        g1 n (ws:wss) = g2 n ws : g1 (n+1) wss
        g2 n [] = []
        g2 n (w:ws) = (startGPSTime w, n) : (stopGPSTime w, n) : g2 n ws

f0 :: [[(GPSTIME, GPSTIME)]] -> [(GPSTIME, Int)]
f0 gss = sort . concat $ g1 1 gss
  where g1 n [] = []
        g1 n (gs:gss) = g2 n gs : g1 (n+1) gss
        g2 n [] = []
        g2 n (g:gs) = (fst g, n) : (snd g, n) : g2 n gs

