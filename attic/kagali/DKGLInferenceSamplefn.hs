
module KAGALIUtils
(dKGLInferenceSamplefnCore)
where


import qualified Data.Vector.Storable as VS (Vector,  length,  unsafeWith,  unsafeFromForeignPtr0, map)
import Foreign.ForeignPtr (ForeignPtr,  newForeignPtr_)
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.C.Types
import System.IO.Unsafe (unsafePerformIO)


dKGLInferenceSamplefn :: Double -> VS.Vector Double
dKGLInferenceSamplefn

dKGLInferenceSamplefnCore :: VS.Vector CDouble -> Int -> CDouble
dKGLInferenceSamplefnCore Vout olen input
  = unsafePerformIO $ allocaArray olen $ \ptrOut ->
    do c_DKGLInferenceSamplefn ptrOut input
       newForeignPtr_ ptrOut >>= \foreignptrOutput ->
         return $ VS.unsafeFromForeignPtr0 foreignptrOutput olen


reign import ccall "hogehoge.h DKGLInferenceSamplefn" c_DKGLInferenceSamplefn :: Ptr CDouble -> CDouble -> IO()

