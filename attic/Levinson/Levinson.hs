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


levinson :: [Double] -> Int -> IO [Double]
levinson r p = do
	let rv = (p+1) |> r :: Vector Double
			partialrv = subVector 1 p rv
			autCorr = toList $ join [reverseVCD partialrv, rv]
			autCorrFloat = map realToFrac autCorr :: [CFloat]
			rvFloat = map realToFrac (toList rv) :: [CFloat]
			out = replicate (p+1) 0

	ptr_autCorrFloat <- withArrayLen autCorrFloat $ \len ptr_tmp -> do
		return ptr_tmp
	ptr_rvFloat <- withArrayLen rvFloat $ \len ptr_tmp ->
		return ptr_tmp
	ptr_outFloat <- withArrayLen out $ \len ptr_tmp -> do
		return ptr_tmp

	c_nr_toeplz ptr_autCorrFloat ptr_outFloat ptr_rvFloat (p+1)
	outFloat <- peekArray (p+1) ptr_outFloat

	let retval =	map realToFrac outFloat :: [Double]
	return retval


--applyConj :: Vector (Complex Double ) -> Vector (Complex)
--applyConj = mapVector conj

reverseVCD :: Vector Double -> Vector Double
reverseVCD = fromList . reverse . toList



foreign import ccall unsafe "nr.h toeplz" c_nr_toeplz :: Ptr CFloat -> Ptr CFloat ->	Ptr CFloat -> Int -> IO()



