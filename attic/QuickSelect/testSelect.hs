
import System.Environment

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Algorithms.Heap as H

main = do
  let xs = [2,3,6,7,5,1,4,8,9]
  mapM (\idx -> do
           let xv = V.fromList $ xs
           print $ V.head (vecSelect idx xv)
       ) [0..length xs - 1]
  

vecSelect :: Int -> V.Vector Int -> V.Vector Int
vecSelect k vec = V.modify (flip H.select (k+1)) vec

