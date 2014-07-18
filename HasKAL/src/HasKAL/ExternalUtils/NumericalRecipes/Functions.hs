{-# LANGUAGE ForeignFunctionInterface #-}

module HasKAL.ExternalUtils.NumericalRecipes.Functions
( nr_toeplz
) where

import Foreign.Ptr(Ptr)
import Foreign.C.Types(CFloat)
import System.IO.Unsafe(unsafePerformIO)
import Foreign.Marshal.Array (withArray, peekArray, allocaArray)
import HasKAL.Misc.StrictMapping
import Control.Monad()  --((>>=))

nr_toeplz :: [Float] -> [Float] -> Int -> [Float]
nr_toeplz r y p = unsafePerformIO $ do
  let r'= 0:map realToFrac r :: [CFloat]
      y'= 0:map realToFrac y :: [CFloat]

  withArray r' $ \ptrr ->
    withArray y' $ \ptry ->
    allocaArray (p+1) $ \ptrx ->
    do c_nr_toeplz ptrr ptry ptrx p
       tmpretval <- peekArray (p+1) ptrx >>= \list_x ->
         forM' list_x $ \x ->return (realToFrac x :: Float)
       return $ tail tmpretval


foreign import ccall unsafe "nr.h toeplz" c_nr_toeplz :: Ptr CFloat -> Ptr CFloat ->  Ptr CFloat -> Int -> IO()


