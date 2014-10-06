{-*********************************
 *RngMedian.hs
 *Created:2014/07/18
 *Author:Mitsuhiro Asano
 **********************************
Last Modified: 2014/10/06 16:37:59
-}

module HasKAL.LineUtils.LineRemoval.RngMedian(
rngMed
)
where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Array
import System.IO
import Data.List.Split 
import System.IO.Unsafe


rngMed :: [Double] -> Int -> Int -> [Double]
rngMed input lentser nblocks = do
       let inputSub = d2cd input
       cd2d $ rngMedCore inputSub lentser nblocks 

rngMedCore :: [CDouble] -> Int -> Int -> [CDouble]
rngMedCore input lentser nblocks 
       = unsafePerformIO $ withArray input $ \ptrInput ->
       	 allocaArray lentser $ \ptrOutput ->
	 do c_rng_med ptrInput lentserSub nblocksSub ptrOutput
	    peekArray lentser ptrOutput
       	    where lentserSub = itow32 lentser
	    	  nblocksSub = itow32 nblocks

itow32 :: Int -> CInt
itow32 = fromIntegral

d2cd :: [Double] -> [CDouble]
d2cd = map realToFrac

cd2d :: [CDouble] -> [Double]
cd2d = map realToFrac


foreign import ccall "rng_median.h rng_med" c_rng_med :: Ptr CDouble -> CInt -> CInt -> Ptr CDouble -> IO()

--In C language : rng_med(double *data,int lendata,int nblocks,double *after_med)


