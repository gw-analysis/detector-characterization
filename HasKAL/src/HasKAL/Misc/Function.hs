module HasKAL.Misc.Function
  ( mkChunksV
  , mkChunksL
  ) where

import Data.Vector.Storable as VS


mkChunksV :: VS.Vector Double -> Int -> [VS.Vector Double]
mkChunksV vIn n = mkChunksVCore vIn n (VS.length vIn `div` n)
  where
    mkChunksVCore _ _ 0 = []
    mkChunksVCore vIn n m 
      = VS.slice 0 n vIn :  mkChunksVCore (VS.drop n vIn) n (m-1)


mkChunksL :: [Double] -> Int -> [[Double]]
mkChunksL lIn n = mkChunksLCore lIn n (Prelude.length lIn `div` n)
  where
    mkChunksLCore _ _ 0 = []
    mkChunksLCore lIn n m 
      = Prelude.take n lIn :  mkChunksLCore (Prelude.drop n lIn) n (m-1)


