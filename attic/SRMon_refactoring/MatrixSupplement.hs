{-******************************************
  *     File Name: MatrixSupplement.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/12/04 14:20:37
  *******************************************-}

module MatrixSupplement (
   convertS2U
  ,convertU2S
  ,mapRows0
  ,mapColumns0
  ,mapRows1
  ,mapColumns1
) where


-- Storable type
import qualified Data.Packed.Matrix as M

-- Unbox type
import Data.Vector.Unboxed
import Data.Matrix.Unboxed

import qualified Control.Monad as CM (forM)
import System.IO.Unsafe (unsafePerformIO)

-- convert type
convertS2U :: (M.Element a, Unbox a) => M.Matrix a -> Matrix a
convertS2U mat = fromVector rowNum colNum $ convert $ M.flatten mat
  where rowNum = M.rows mat
        colNum = M.cols mat

convertU2S :: (Unbox a, M.Element a) => Matrix a -> M.Matrix a
convertU2S mat = M.reshape colNum $ convert $ flatten mat
  where colNum = cols mat


-- map for Matrix
mapRows0 :: (Unbox a) => (Vector a -> a) -> Matrix a -> Vector a
mapRows0 fx mat = unsafePerformIO $ forM idxV $ \idx -> return $ fx $ takeRow idx mat
  where idxV = fromList [0..(rows mat)-1]

mapColumns0 :: (Unbox a) => (Vector a -> a) -> Matrix a -> Vector a
mapColumns0 fx mat = unsafePerformIO $ forM idxV $ \idx -> return $ fx $ takeColumn idx mat
  where idxV = fromList [0..(cols mat)-1]

mapRows1 :: (Unbox a) => (Vector a -> Vector a -> Vector a) -> Vector a -> Matrix a -> Matrix a
mapRows1 fx vec mat = fromRows $ unsafePerformIO $ CM.forM idxL $ \idx -> return $ fx vec $ takeRow idx mat
  where idxL = [0..(rows mat)-1]

mapColumns1 :: (Unbox a) => (Vector a -> Vector a -> Vector a) -> Vector a -> Matrix a -> Matrix a
mapColumns1 fx vec mat = fromColumns $ unsafePerformIO $ CM.forM idxL $ \idx -> return $ fx vec $ takeColumn idx mat
  where idxL = [0..(cols mat)-1]
        
        

