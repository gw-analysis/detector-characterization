{-******************************************
  *     File Name: SMatrixMapping.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/12/05 16:35:26
  *******************************************-}

-- map functions for Storable Matrix
module HasKAL.Misc.SMatrixMapping (
   convertS2U
  ,convertU2S
  ,mapRows0
  ,mapCols0
  ,mapRows1
  ,mapCols1
) where


import qualified Control.Monad as CM (forM)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Vector.Generic as G

{-- Storable type --}
import Data.Packed.Matrix
import Data.Packed.Vector

{-- Unbox type --}
import qualified Data.Vector.Unboxed as V
import qualified Data.Matrix.Unboxed as M

{-- matrix type converter --}
convertS2U :: (Element a, V.Unbox a) => Matrix a -> M.Matrix a
convertS2U mat = M.fromVector rowNum colNum $ V.convert $ flatten mat
  where rowNum = rows mat
        colNum = cols mat

convertU2S :: (V.Unbox a, Element a) => Matrix a -> Matrix a
convertU2S mat = reshape colNum $ V.convert $ flatten mat
  where colNum = cols mat


{-- map functions --}
mapRows0 :: (Element t) => (Vector t -> t) -> Matrix t -> Vector t
mapRows0 fx mat = unsafePerformIO $ G.forM idxV $ \idx -> return $ fx $ getVecs idx mat
  where idxV = fromList [0..(rows mat)-1]
        getVecs idx mat = flatten $ takeRows 1 $ dropRows idx mat

mapCols0 :: (Element t) => (Vector t -> t) -> Matrix t -> Vector t
mapCols0 fx mat = unsafePerformIO $ G.forM idxV $ \idx -> return $ fx $ getVecs idx mat
  where idxV = fromList [0..(cols mat)-1]
        getVecs idx mat = flatten $ takeColumns 1 $ dropColumns idx mat
  
mapRows1 :: (Element t) => (Vector t -> Vector t -> Vector t) -> Vector t -> Matrix t -> Matrix t
mapRows1 fx vec mat = fromRows $ unsafePerformIO $ CM.forM idxL $ \idx -> return $ fx vec $ getVecs idx mat
  where idxL = [0..(rows mat)-1]
        getVecs idx mat = flatten $ takeRows 1 $ dropRows idx mat

mapCols1 :: (Element t) => (Vector t -> Vector t -> Vector t) -> Vector t -> Matrix t -> Matrix t
mapCols1 fx vec mat = fromColumns $ unsafePerformIO $ CM.forM idxL $ \idx -> return $ fx vec $ getVecs idx mat
  where idxL = [0..(cols mat)-1]
        getVecs idx mat = flatten $ takeColumns 1 $ dropColumns idx mat

