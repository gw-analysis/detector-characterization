
module HasKAL.ExternalUtils.KAGALI.KAGALIUtils
 (  dKGLIterativeLeastSquare2DNewton
  , butterBandPass
  , nha
  , formatNHA
 ) where

import qualified Data.Vector.Storable as VS (Vector,  length,  unsafeWith,  unsafeFromForeignPtr0, map, slice, fromList)
import Foreign.ForeignPtr (newForeignPtr_)
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.C.Types
import System.IO.Unsafe (unsafePerformIO)
import Data.Packed.Matrix (toColumns, fromRows)

{- exposed functions -}
judge :: Double->Double->Double->Int->Bool
judge flow fhigh fs order
   | (flow < fhigh) &&
     ((flow == 0) ||
     (((flow/fs)**(fromIntegral order) > 1e-14) &&
     (((fs/2-fhigh)/fs)**(fromIntegral order) > 1e-14))) = True
   | otherwise = False


butterBandPass :: VS.Vector Double->Double->Double->Double->Int->Either String (VS.Vector Double)
butterBandPass inputV fs flow fhigh order =
  case judge flow fhigh fs order of 
   False -> Left "butterBandPass: please check that f_low < f_high, f_low is not too small, and f_high is not too high."
   True -> Right output
     where inputV' = d2cdV inputV
           npoint = VS.length inputV :: Int
           fs' = realToFrac fs
           flow' = realToFrac flow
           fhigh' = realToFrac fhigh
           output = cd2dV $ butterBandPassCore inputV' npoint fs' flow' fhigh' order


dKGLIterativeLeastSquare2DNewton :: VS.Vector Double    -- ^ Input Vector (frame)
                                 -> Double              -- ^ Sampling frequency (fs)
                                 -> Int                 -- ^ # of elements in Onput Vector (nsig)
                                 -> ( VS.Vector Double  -- ^ Output Vector (Afit)
                                    , VS.Vector Double  -- ^ Output Vector (ffit)
                                    , VS.Vector Double) -- ^ Output Vector (pfit)
dKGLIterativeLeastSquare2DNewton frame fs nsig = do 
  let frame' = d2cdV frame
      nframe = VS.length frame :: Int
      fs' = realToFrac fs
  let (out1,out2,out3) = dKGLIterativeLeastSquare2DNewtonCore frame' nframe nsig fs'
   in (cd2dV out1, cd2dV out2, cd2dV out3)


nha :: VS.Vector Double->Double->Int->Int->Int->Int->Int->[(Double, VS.Vector Double, VS.Vector Double, VS.Vector Double)]
nha datV fs nsig nframe nshift nstart nend = retVal
  where retVal = zipWith3 (\v w (x, y, z) -> ((v+w)/2, x, y, z)) tstart tend result
        tstart = map ( (/fs) . fromIntegral ) nIdx
        tend = map ( (/fs) . fromIntegral . (+nframe) ) nIdx
        nIdx = [nstart, nstart + nshift .. nstop]
        nstop = min (VS.length datV - nframe) nend
        result =
          map ( (\frameV -> dKGLIterativeLeastSquare2DNewton frameV fs nsig) . (\kstart -> VS.slice kstart nframe datV) ) nIdx


formatNHA :: [(Double, VS.Vector Double, VS.Vector Double, VS.Vector Double)] -> [[(VS.Vector Double, VS.Vector Double)]]
formatNHA input = output
  where tVec = VS.fromList $ map (\(x, _, _, _) -> x) input
        aVecL = toColumns.fromRows $ map (\(_, y, _, _) -> y) input
        fVecL = toColumns.fromRows $ map (\(_, _, z, _) -> z) input
        pVecL = toColumns.fromRows $ map (\(_, _, _, w) -> w) input
        output = map (map (\vl -> (tVec, vl)) ) [aVecL, fVecL, pVecL]


{- internal functions -}
butterBandPassCore :: VS.Vector CDouble  -- ^ Input Vector
                          -> Int                -- ^ # of elements in Input Vector
                          -> CDouble            -- ^ sampling frequency
                          -> CDouble            -- ^ lower cutoff frequency
                          -> CDouble            -- ^ higher cutoff frequency
                          -> Int                -- ^ filter order
                          -> VS.Vector CDouble  -- ^ Output Vector
butterBandPassCore inputV npoint fs flow fhigh order
  = unsafePerformIO $ VS.unsafeWith inputV $ \ptrIn ->
   allocaArray npoint $ \ptrOut ->
--   do c_DKGLButterworthBandPassFilter ptrOut ptrIn (fromIntegral npoint) fs flow fhigh (fromIntegral order)
   do c_DKGLButterworthBandPassSOSFilter ptrOut ptrIn (fromIntegral npoint) fs flow fhigh (fromIntegral order)
      newForeignPtr_ ptrOut >>= \foreignptrOutput ->
         return $ VS.unsafeFromForeignPtr0 foreignptrOutput npoint


dKGLIterativeLeastSquare2DNewtonCore :: VS.Vector CDouble       -- ^ Input Vector (frame)
                                     -> Int                     -- ^ # of elements in Input Vector (nframe)
                                     -> Int                     -- ^ # of elements in Output Vector (nsig)
                                     -> CDouble                 -- ^ fs
                                     -> ( VS.Vector CDouble     -- ^ Output Vector (Afit)
                                        , VS.Vector CDouble     -- ^ Output Vector (ffit)
                                        , VS.Vector CDouble)    -- ^ Output Vector (pfit)
dKGLIterativeLeastSquare2DNewtonCore frame' nframe nsig fs'
  = unsafePerformIO $ VS.unsafeWith frame' $ \ptrIn ->
   allocaArray nsig $ \ptrOut1 ->
     allocaArray nsig $ \ptrOut2 ->
       allocaArray nsig $ \ptrOut3 ->
         do c_DKGLIterativeLeastSquare2DNewton ptrOut1 ptrOut2 ptrOut3 ptrIn fs' (fromIntegral nframe) (fromIntegral nsig)
            newForeignPtr_ ptrOut1 >>= \foreignptrOutput1 ->
              newForeignPtr_ ptrOut2 >>= \foreignptrOutput2 ->
                newForeignPtr_ ptrOut3 >>= \foreignptrOutput3 ->
                return $ ( VS.unsafeFromForeignPtr0 foreignptrOutput1 nsig
                         , VS.unsafeFromForeignPtr0 foreignptrOutput2 nsig
                         , VS.unsafeFromForeignPtr0 foreignptrOutput3 nsig)

{-
itow32 :: Int -> CInt
itow32 = fromIntegral
-}


d2cdV :: VS.Vector Double -> VS.Vector CDouble
d2cdV = VS.map realToFrac

cd2dV :: VS.Vector CDouble -> VS.Vector Double
cd2dV = VS.map realToFrac


--foreign import ccall "DKGLUtils.h DKGLButterworthBandPassFilter" c_DKGLButterworthBandPassFilter :: Ptr CDouble -- ^ output pointer
foreign import ccall "DKGLUtils.h DKGLButterworthBandPassSOSFilter" c_DKGLButterworthBandPassSOSFilter :: Ptr CDouble -- ^ output pointer
                                                                                -> Ptr CDouble -- ^ input pointer
                                                                                -> CInt        -- ^ # of elements in input
                                                                                -> CDouble     -- ^ sampling frequency [Hz]
                                                                                -> CDouble     -- ^ lower cutoff frequency [Hz]
                                                                                -> CDouble     -- ^ higher cutoff frequency [Hz]
                                                                                -> CInt        -- ^ filter order
                                                                                -> IO()

foreign import ccall "DKGLUtils.h DKGLIterativeLeastSquare2DNewton" c_DKGLIterativeLeastSquare2DNewton :: Ptr CDouble -- ^ input pointer (frame)
                                                                                -> Ptr CDouble -- ^ output pointer (Afit)
                                                                                -> Ptr CDouble -- ^ output pointer (ffit)
                                                                                -> Ptr CDouble -- ^ output pointer (pfit)
                                                                                -> CDouble     -- ^ fs
                                                                                -> CInt        -- ^ # of elements in input (nframe)
                                                                                -> CInt        -- ^ # of elements in output (nsig)
                                                                                -> IO()
