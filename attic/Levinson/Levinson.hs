{-# LANGUAGE ForeignFunctionInterface #-}

module Levinson
( levinson
--,
) where

import Numeric.LinearAlgebra
--import Numeric.GSL.Fourier
import Foreign.Ptr(Ptr)
import Foreign.C.Types (CFloat)
import Foreign.Marshal.Array (withArray, allocaArray, peekArray)
import System.IO.Unsafe(unsafePerformIO)


levinson :: [Double] -> Int -> [Double]
levinson r p = unsafePerformIO $ do
  case inputCheck (length r) p of Nothing -> error "r should be >= 2*p+1"
--                         Just _  ->
  let rv = p |> r :: Vector Double
      partialrv = subVector 1 p rv
      autCorr = toList $ join [reverseVCD partialrv, rv]
      autCorrFloat = map realToFrac (0:autCorr) :: [CFloat]
      rvFloat = map realToFrac (0:toList rv) :: [CFloat]

  withArray autCorrFloat $ \ptrautCorrFloat ->
    withArray rvFloat $ \ptrrvFloat ->
    allocaArray p $ \ptroutFloat ->
    do c_nr_toeplz ptrautCorrFloat ptroutFloat ptrrvFloat p
       outFloat <- peekArray p ptroutFloat
       return . tail $ cf2d (1:outFloat)

--applyConj :: Vector (Complex Double ) -> Vector (Complex)
--applyConj = mapVector conj

reverseVCD :: Vector Double -> Vector Double
reverseVCD = fromList . reverse . toList

inputCheck :: Int -> Int -> Maybe Int
inputCheck r p = if r<2*p+1 then Nothing else Just (p+1)

cf2d :: [CFloat] -> [Double]
cf2d xs = map realToFrac xs :: [Double]

foreign import ccall unsafe "nr.h toeplz" c_nr_toeplz :: Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Int -> IO()



