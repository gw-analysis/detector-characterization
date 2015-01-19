{- |
Module      : HasKAL.Misc.UMatrixMapping
Description : This is documentation tests.
Copyright   : (c) WhoAmI, 2014
License     : ???
Maintainer  : hoge@hoge.com
Stability   : test
Portability : POSIX

map functions for Unboxed Matrix
-}

module HasKAL.Misc.UMatrixMapping (
   convertS2U
  ,convertU2S
  ,mapRows0
  ,mapCols0
  ,mapRows1
  ,mapCols1
) where


import qualified Control.Monad as CM (forM)
import System.IO.Unsafe (unsafePerformIO)

{-- Unbox type --}
import Data.Vector.Unboxed
import Data.Matrix.Unboxed hiding (convert, forM)

{-- Storable type --}
import qualified Data.Packed.Matrix as M


{-- matrix type converter --}
-- | Convert from Data.Packed.Matrix to Data.Matrix.Unboxed
convertS2U :: (M.Element a, Unbox a) => M.Matrix a -> Matrix a
convertS2U mat = fromVector (rowNum, colNum) $ convert $ M.flatten mat
  where rowNum = M.rows mat
        colNum = M.cols mat

-- | Convert from Data.Matrix.Unboxed to Data.Packed.Matrix
convertU2S :: (Unbox a, M.Element a) => Matrix a -> M.Matrix a
convertU2S mat = M.reshape colNum $ convert $ flatten mat
  where colNum = cols mat


{-- map functions --}
mapRows0 :: (Unbox a) => (Vector a -> a) -> Matrix a -> Vector a
mapRows0 fx mat = unsafePerformIO $ forM idxV $ \idx -> return $ fx $ takeRow mat idx
  where idxV = fromList [0..(rows mat)-1]

mapCols0 :: (Unbox a) => (Vector a -> a) -> Matrix a -> Vector a
mapCols0 fx mat = unsafePerformIO $ forM idxV $ \idx -> return $ fx $ takeColumn mat idx
  where idxV = fromList [0..(cols mat)-1]

mapRows1 :: (Unbox a) => (Vector a -> Vector a -> Vector a) -> Vector a -> Matrix a -> Matrix a
mapRows1 fx vec mat = fromRows $ unsafePerformIO $ CM.forM idxL $ \idx -> return $ fx vec $ takeRow mat idx
  where idxL = [0..(rows mat)-1]

mapCols1 :: (Unbox a) => (Vector a -> Vector a -> Vector a) -> Vector a -> Matrix a -> Matrix a
mapCols1 fx vec mat = fromColumns $ unsafePerformIO $ CM.forM idxL $ \idx -> return $ fx vec $ takeColumn mat idx
  where idxL = [0..(cols mat)-1]
        
        

