module HasKAL.Misc.Function
  ( mkChunksV
  , mkChunksL
  , mkChunksW
  ) where

import Data.Vector.Storable as VS
import qualified HasKAL.WaveUtils.Data as HAL


mkChunksV :: VS.Vector Double -> Int -> Int -> [VS.Vector Double]
mkChunksV vIn on n = mkChunksVCore vIn on n ((VS.length vIn -on) `div` (n-on))
  where
    mkChunksVCore _ _ _ 0 = []
    mkChunksVCore vIn on n m
      = VS.slice 0 n vIn :  mkChunksVCore (VS.drop (n-on) vIn) on n (m-1)


mkChunksL :: [Double] -> Int -> Int -> [[Double]]
mkChunksL lIn on n = mkChunksLCore lIn on n ((Prelude.length lIn -on) `div` (n-on))
  where
    mkChunksLCore _ _ _ 0 = []
    mkChunksLCore lIn on n m
      = Prelude.take n lIn :  mkChunksLCore (Prelude.drop (n-on) lIn) on n (m-1)

mkChunksW :: HAL.WaveData -> Int -> Int -> [HAL.WaveData]
mkChunksW wIn on n = mkChunksLCore wIn on n ((VS.length (HAL.gwdata wIn) -on) `div` (n-on))
  where
    mkChunksLCore _ _ _ 0 = []
    mkChunksLCore wIn on n m
      = HAL.takeWaveData n wIn :  mkChunksLCore (HAL.dropWaveData (n-on) wIn) on n (m-1)
