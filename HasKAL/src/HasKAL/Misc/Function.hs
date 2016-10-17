module HasKAL.Misc.Function
  ( mkChunks
  ) where

import Data.Vector.Storable as VS


mkChunks :: VS.Vector Double -> Int -> [VS.Vector Double]
mkChunks vIn n = mkChunksCore vIn n (VS.length vIn `div` n)
  where
    mkChunksCore _ _ 0 = []
    mkChunksCore vIn n m 
      = VS.slice 0 n vIn :  mkChunksCore (VS.drop n vIn) n (m-1)


