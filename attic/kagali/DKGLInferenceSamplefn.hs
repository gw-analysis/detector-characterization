

--
--
--
--


module KAGALIUtils
(dKGLInferenceSamplefn)
where


import qualified Data.Vector.Storable as VS (Vector,  length,  unsafeWith,  unsafeFromForeignPtr0, map)
import Foreign.ForeignPtr (newForeignPtr_)
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.C.Types
import System.IO.Unsafe (unsafePerformIO)


{- exposed functions -}
dKGLInferenceSamplefn :: VS.Vector Double  -- ^ Input Vector
                      -> VS.Vector Double  -- ^ Output Vector
dKGLInferenceSamplefn vIn = do
  let vIn' = d2cdV vIn
      ilen= VS.length vIn :: Int
      olen= ilen
  cd2dV $ dKGLInferenceSamplefnCore vIn' ilen olen



{- internal functions -}
dKGLInferenceSamplefnCore :: VS.Vector CDouble  -- ^ Input Vector
                          -> Int                -- ^ # of elements in Input Vector
                          -> Int                -- ^ # of elements in Input Vector
                          -> VS.Vector CDouble  -- ^ Output Vector
dKGLInferenceSamplefnCore input ilen olen
  = unsafePerformIO $ VS.unsafeWith input $ \ptrIn ->
   allocaArray olen $ \ptrOut ->
   do c_DKGLInferenceSamplefn ptrOut (fromIntegral olen) ptrIn (fromIntegral ilen)
      newForeignPtr_ ptrOut >>= \foreignptrOutput ->
         return $ VS.unsafeFromForeignPtr0 foreignptrOutput olen


d2cdV :: VS.Vector Double -> VS.Vector CDouble
d2cdV = VS.map realToFrac


cd2dV :: VS.Vector CDouble -> VS.Vector Double
cd2dV = VS.map realToFrac


foreign import ccall "hogehoge.h DKGLInferenceSamplefn" c_DKGLInferenceSamplefn :: Ptr CDouble -- ^ input pointer
                                                                                -> CInt        -- ^ # of elements in input
                                                                                -> Ptr CDouble -- ^ output pointer
                                                                                -> CInt        -- ^ # of elements in output
                                                                                -> IO()
