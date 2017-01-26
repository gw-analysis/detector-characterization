{-# LANGUAGE CPP,  ForeignFunctionInterface #-}
module KAGALIUtils_new
 (  dKGLChirpletMain
 ) where


import qualified Data.Vector.Storable as VS (Vector,  length,  unsafeWith,  unsafeFromForeignPtr0, map, slice, fromList, toList)
import Foreign.ForeignPtr (newForeignPtr_)
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.C.Types
import System.IO.Unsafe (unsafePerformIO)
import Numeric.LinearAlgebra.Data (toColumns, fromRows)

{- exposed functions -}
dKGLChirpletMain :: VS.Vector Double    -- ^ Input Vector (frame)
                 -> Double              -- ^ Sampling frequency (fs)
                 -> Double              -- ^ maxLength (alpha)
                 -> Int                 -- ^ # of paths used (ipath)
                 -> ( VS.Vector Double  -- ^ Output Vector (freq)
                    , VS.Vector Double) -- ^ Output Vector (cost)
dKGLChirpletMain frame fs alpha ipath = do
  let frame' = d2cdV frame
      nframe = VS.length frame :: Int
      fs' = realToFrac fs
      alpha' = realToFrac alpha
  let (out1, out2) = dKGLChirpletMainCore frame' nframe ipath fs' alpha'
   in (cd2dV out1, cd2dV out2)


chirplet_daily_org :: VS.Vector Double
                   -> Double
                   -> Double
                   -> Int
                   -> Int
                   -> Int
                   -> Int
                   -> Int
                   -> Double
                   -> [(Double, VS.Vector Double, VS.Vector Double)]
chirplet_daily_org datV fs alpha ipath nframe nshift nstart nend t0 = retVal
  where retVal = zipWith3 (\v w (x,y) -> ((v+w)/2, x, y)) tstart tend result
        tstart = map ( (/3600.0) . (+t0) . (/fs) . fromIntegral) nIdx
        tend = map ( (/3600.0) . (+t0) . (/fs) . fromIntegral . (+nframe) ) nIdx
        nIdx = [nstart, nstart + nshift .. nstop]
        nstop = min (VS.length datV - nframe) nend
        result =
          map ( (\frameV -> dKGLChirpletMain frameV fs alpha ipath) . (\kstart -> VS.slice kstart nframe datV) ) nIdx


chirplet_daily :: VS.Vector Double
               -> Double
               -> Double
               -> Int
               -> Int
               -> Int
               -> Int
               -> Int
               -> Double
               -> [(Double, VS.Vector Double)]
chirplet_daily datV fs alpha ipath nframe nshift nstart nend t0 = retVal
  where outV = chirplet_daily_org datV fs alpha ipath nframe nshift nstart nend t0
        retVal = [(a,b) | (a,b,c) <- outV
                            , null [ e | e <- VS.toList c
                                       , isNaN e]]


{- internal functions -}
dKGLChirpletMainCore :: VS.Vector CDouble       -- ^ Input Vector (frame)
                     -> Int                     -- ^ # of elements in Input Vector (nframe)
                     -> Int                     -- ^ # of paths used (ipath)
                     -> CDouble                 -- ^ fs
                     -> CDouble                 -- ^ alpha (maxLength)
                     -> (VS.Vector CDouble, VS.Vector CDouble) -- ^ (Output Vector (freq), Output Vector (cost))
dKGLChirpletMainCore frame' nframe ipath fs' alpha'
  = unsafePerformIO $ VS.unsafeWith frame' $ \ptrIn ->
   allocaArray ipath $ \ptrOut1 ->
     allocaArray ipath $ \ptrOut2 ->
     do c_DKGLChirpletMain ptrOut1 ptrOut2 ptrIn fs' alpha' (fromIntegral ipath) (fromIntegral nframe)
        newForeignPtr_ ptrOut1 >>= \foreignptrOutput1 ->
          newForeignPtr_ ptrOut2 >>= \foreignptrOutput2 ->
            return $ ( VS.unsafeFromForeignPtr0 foreignptrOutput1 ipath
                     , VS.unsafeFromForeignPtr0 foreignptrOutput2 ipath
                     )


d2cdV :: VS.Vector Double -> VS.Vector CDouble
d2cdV = VS.map realToFrac


cd2dV :: VS.Vector CDouble -> VS.Vector Double
cd2dV = VS.map realToFrac


foreign import ccall "DKGLUtils_new.h DKGLChirpletMain" c_DKGLChirpletMain :: Ptr CDouble -- ^ input pointer (frame)
                                                    -> Ptr CDouble -- ^ output pointer (freq array)
                                                    -> Ptr CDouble -- ^ output pointer (cost value)
                                                    -> CDouble     -- ^ sampling frequency [Hz]
                                                    -> CDouble     -- ^ maxLength (alpha)
                                                    -> CInt        -- ^ # of paths used (ipath)
                                                    -> CInt        -- ^ # of elements in input (nframe)
                                                    -> IO()
