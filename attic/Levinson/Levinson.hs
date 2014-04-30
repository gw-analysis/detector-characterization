{-# LANGUAGE ForeignFunctionInterface #-}

module Levinson
( levinson
--,
) where

import Numeric.LinearAlgebra
--import Numeric.GSL.Fourier
import Foreign.Ptr(Ptr)
import Foreign.C.Types (CFloat)
import Foreign.Marshal.Array (withArrayLen, peekArray)
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
      out = replicate p 0

  ptr_autCorrFloat <- withArrayLen autCorrFloat $ \len ptr_tmp -> do
    return ptr_tmp
  ptr_rvFloat <- withArrayLen rvFloat $ \len ptr_tmp ->
    return ptr_tmp
  ptr_outFloat <- withArrayLen out $ \len ptr_tmp -> do
    return ptr_tmp

  c_nr_toeplz ptr_autCorrFloat ptr_outFloat ptr_rvFloat p
  outFloat <- peekArray p ptr_outFloat

  return (tail (map realToFrac (1:outFloat)) :: [Double])


--applyConj :: Vector (Complex Double ) -> Vector (Complex)
--applyConj = mapVector conj

reverseVCD :: Vector Double -> Vector Double
reverseVCD = fromList . reverse . toList

inputCheck :: Int -> Int -> Maybe Int
inputCheck r p = case (r<2*p+1) of True -> Nothing
                                   False -> Just (p+1)



foreign import ccall unsafe "nr.h toeplz" c_nr_toeplz :: Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Int -> IO()



