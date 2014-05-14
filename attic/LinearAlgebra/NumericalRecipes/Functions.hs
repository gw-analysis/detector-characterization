{-# LANGUAGE ForeignFunctionInterface #-}

module NumericalRecipes.Functions
( nr_toeplz
) where

import Foreign.Ptr(Ptr)
import Foreign.C.Types(CFloat)
import System.IO.Unsafe(unsafePerformIO)
import Foreign.Marshal.Array (withArrayLen, peekArray)
import StrictMapping.StrictMapping
import Control.Monad()  --((>>=))

nr_toeplz :: [Float] -> [Float] -> Int -> [Float]
nr_toeplz r y p = unsafePerformIO $ do
  let x = replicate p (0::CFloat)
      r'= map realToFrac r :: [CFloat]
      y'= map realToFrac y :: [CFloat]

  ptr_r <- withArrayLen r' $ \len ptr_tmp -> do
    return ptr_tmp
  ptr_y <- withArrayLen y' $ \len ptr_tmp ->
    return ptr_tmp
  ptr_x <- withArrayLen x $ \len ptr_tmp -> do
    return ptr_tmp
  c_nr_toeplz ptr_r ptr_y ptr_x p
  tmpretval <- peekArray p ptr_x >>= \list_x ->
    forM' list_x $ \x ->return (realToFrac x :: Float)
  return (1:(tail tmpretval))


foreign import ccall unsafe "nr.h toeplz" c_nr_toeplz :: Ptr CFloat -> Ptr CFloat ->  Ptr CFloat -> Int -> IO()


