{-# LANGUAGE CPP,  ForeignFunctionInterface #-}
module KAGALIUtils_new
 (  dKGLChirplet
 ) where


import qualified Data.Vector.Storable as VS (Vector,  length,  unsafeWith,  unsafeFromForeignPtr0, map, slice, fromList, toList)
import Foreign.ForeignPtr (newForeignPtr_)
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.C.Types
import System.IO.Unsafe (unsafePerformIO)
import Numeric.LinearAlgebra.Data (toColumns, fromRows)

{- exposed functions -}
dKGLChirplet :: VS.Vector Double    -- ^ Input Vector (frame)
             -> Double              -- ^ Sampling frequency (fs)
             -> Double              -- ^ maxLength (alpha)
             -> Int                 -- ^ # of paths used (ipath)
             -> ( VS.Vector Double  -- ^ Output Vector (time)
                , VS.Vector Double) -- ^ Output Vector (freq)
dKGLChirplet frame fs alpha ipath = do 
  let frame' = d2cdV frame
      nframe = VS.length frame :: Int
      fs' = realToFrac fs
      alpha' = realToFrac alpha
  let (out1,out2) = dKGLChirpletCore frame' nframe ipath fs' alpha'
   in (cd2dV out1, cd2dV out2)

{- internal functions -}
dKGLChirpletCore :: VS.Vector CDouble       -- ^ Input Vector (frame)
                 -> Int                     -- ^ # of elements in Input Vector (nframe)
                 -> Int                     -- ^ # of paths used (ipath)
                 -> CDouble                 -- ^ fs
                 -> CDouble                 -- ^ alpha (maxLength)
                 -> ( VS.Vector CDouble     -- ^ Output Vector (time)
                    , VS.Vector CDouble)    -- ^ Output Vector (freq)
dKGLChirpletCore frame' nframe ipath fs' alpha'
  = unsafePerformIO $ VS.unsafeWith frame' $ \ptrIn ->
   allocaArray 1 $ \ptrOut1 ->
     allocaArray 1 $ \ptrOut2 ->
     do c_DKGLChirpletMain ptrOut1 ptrOut2 ptrIn fs' alpha' (fromIntegral ipath) (fromIntegral nframe)
        newForeignPtr_ ptrOut1 >>= \foreignptrOutput1 ->
          newForeignPtr_ ptrOut2 >>= \foreignptrOutput2 ->
          return $ ( VS.unsafeFromForeignPtr0 foreignptrOutput1 1
                       , VS.unsafeFromForeignPtr0 foreignptrOutput2 1)

d2cdV :: VS.Vector Double -> VS.Vector CDouble
d2cdV = VS.map realToFrac

cd2dV :: VS.Vector CDouble -> VS.Vector Double
cd2dV = VS.map realToFrac


foreign import ccall "DKGLUtils_new.h DKGLChirpletMain" c_DKGLChirpletMain :: Ptr CDouble -- ^ input pointer (frame)
                                                    -> Ptr CDouble -- ^ output pointer (time array)
                                                    -> Ptr CDouble -- ^ output pointer (freq array)
                                                    -> CDouble     -- ^ sampling frequency [Hz]
                                                    -> CDouble     -- ^ maxLength (alpha)
                                                    -> CInt        -- ^ # of paths used (ipath)
                                                    -> CInt        -- ^ # of elements in input (nframe)
                                                    -> IO()