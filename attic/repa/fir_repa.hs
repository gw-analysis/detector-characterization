




-- fir'0 :: Num a => Array Int a -> Array Int a -> [a] -> [a]
-- fir'0 h w []     = y : []
--     where y  = sum [ h!i * w!i | i <- [0..m] ]
--           m  = snd $ bounds h
-- fir'0 h w (x:xs) = y : fir'0 h w' xs
--     where y  = sum [ h!i * w!i | i <- [0..m] ]
--           w' = listArray (0,m) $ x : elems w
--           m  = snd $ bounds h
--
--
--

import Control.Monad.Identity
import Foreign.Ptr
import qualified Foreign.ForeignPtr.Safe as FPS
import qualified Data.Array.Repa as Repa
import qualified Data.Array.Repa.Repr.ForeignPtr as RFP
import Data.Array.Repa
import Data.Vector.Storable as VS
import qualified Numeric.LinearAlgebra as NL
import System.Random
import HasKAL.SignalProcessingUtils.ButterWorth


--main = do

datalen = 1024 :: Int
x = VS.fromList $ Prelude.take datalen $ randomRs (-1,1) $ mkStdGen 1 :: Vector Double
h = VS.fromList $ snd $ butter 6 1024 50 High
w = VS.fromList $ (x NL.@> 0):Prelude.replicate 6 0




fir'0repa :: Repa.Array Repa.U Repa.DIM1 Double -> Repa.Array Repa.U Repa.DIM1 Double -> Repa.Array Repa.U Repa.DIM1 Double -> IO (Repa.Array Repa.U Repa.DIM1 Double)
fir'0repa rh rw rx
  | rx == fromListUnboxed (Z:.(1::Int)) [] = do
    y' <- computeP (rh *^ rw) :: IO (Array U DIM1 Double)
    sumy <- sumAllP y'
    return (fromListUnboxed (Z :.(1::Int)) [sumy])  :: IO (Array U DIM1 Double)
  | otherwise = do
    y' <- computeP (rh *^ rw) :: IO (Array U DIM1 Double)
    let y = (fromListUnboxed (Z :.(1::Int)) [sumAllS y']) :: Array U DIM1 Double
    rw' <- computeP (extract (Z:.(0::Int)) (Z:.(1::Int)) rx
      Repa.++ (extract (Z :.(1::Int)) (Z :.(size (extent rw)-1)) rw)) :: IO (Array U DIM1 Double)
    rx' <- computeP (extract (Z:.(1::Int)) (Z:.(size (extent rx)-1)) rx) :: IO (Array U DIM1 Double)
    output <- fir'0repa rh rw' rx' :: IO (Array U DIM1 Double)
    computeP (y Repa.++ output) :: IO (Array U DIM1 Double)




ptr2repa :: Int -> Ptr Double -> IO (Repa.Array RFP.F Repa.DIM1 Double)
ptr2repa len p = do
  fp <- FPS.newForeignPtr_ p
  return $ RFP.fromForeignPtr (Z :. len) fp


