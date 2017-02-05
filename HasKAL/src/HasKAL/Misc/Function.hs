module HasKAL.Misc.Function
  ( mkChunksV
  , mkChunksL
  ) where

import Data.Vector.Storable as VS


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
