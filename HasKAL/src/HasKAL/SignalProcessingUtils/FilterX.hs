
{-# LANGUAGE ForeignFunctionInterface #-}

module HasKAL.SignalProcessingUtils.FilterX
  ( calcInitCond
  , filterX
  , filterX1d
  , filtfiltX
  , filtfiltX1d
  ) where

import qualified Data.Vector.Storable as VS (Vector, concat, drop, length, slice, unsafeWith, unsafeFromForeignPtr0,map, toList)
import Data.Word
import Foreign.C.Types
import Foreign.C.String
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr_, newForeignPtr)
import Foreign.Ptr
import Foreign.Marshal.Alloc(finalizerFree)
import Foreign.Marshal.Array
import HasKAL.Misc.Function (mkChunksV,mkChunksL)
import Numeric.LinearAlgebra (flipud, fromBlocks, fromList, fromColumns, toColumns, fromRows, toRows, ident, scale, toLists, (><), (<\>), dropRows, rows, takeRows, asRow, trans)
import qualified Numeric.LinearAlgebra.Data as ND
import System.IO.Unsafe


-- | filtfiltX (num, denom) inputV 
filtfiltX :: ([Double], [Double]) -> [VS.Vector Double] -> [VS.Vector Double]
filtfiltX (num, denom) inputV = 
  let nb = length num
      na = length denom
      order = max nb na
      inputM = fromColumns inputV
      -- Use a reflection to extrapolate signal at beginning and end to reduce edge effects
      nEdge = 3 * (order - 1)
      x1_2 = VS.map (*2) $ head . toRows $ inputM
      xf_2 = VS.map (*2) $ last . toRows $ inputM
      xi''   = fromRows (replicate nEdge x1_2) - (flipud . takeRows nEdge . dropRows 1 $ inputM)
      xi'    = toColumns xi''
      xi    = map VS.toList xi'
      xf''   = fromRows (replicate nEdge xf_2)
               - (flipud . takeRows nEdge . dropRows (rows inputM-nEdge) $ inputM)
      xf'   = toColumns xf''
      xf    = map VS.toList xf'
      -- Filter initial reflected signal:
      ic = ((order-1)><1) $ calcInitCond (num,denom)
      (dum,zi) = filterX (num, denom) (map VS.toList (toColumns (ic * takeRows 1 xi''))) "forward" xi'
      -- Use the final conditions of the initial part for the actual signal:
      (ys,zs) = filterX (num, denom) zi "forward" inputV -- "s"teady state
      (yf,zdum) = filterX (num, denom) zs "forward" xf'  -- "f"inal conditions
      -- Filter signal again in reverse order:
      yEdge = asRow $ (fromColumns yf) ND.! (nEdge-1)
      (dum',zf) = filterX (num, denom) (map VS.toList (toColumns (ic * yEdge))) "reverse" yf
   in fst $ filterX (num, denom) zf "reverse" ys


filtfiltX1d :: ([Double], [Double]) -> VS.Vector Double -> VS.Vector Double
filtfiltX1d (num, denom) inputV = head $ filtfiltX (num, denom) [inputV]


filterX1d :: ([Double], [Double]) -> [Double] -> String -> VS.Vector Double -> (VS.Vector Double,[Double])
filterX1d (num, denom) z dir inputV = let a = filterX (num, denom) [z] dir [inputV]
                                       in (head . fst $ a, head . snd $ a)      


-- | filterX (num, denom) z dir inputV
filterX :: ([Double], [Double]) -> [[Double]] -> String -> [VS.Vector Double] -> ([VS.Vector Double], [[Double]])
filterX (num, denom) z dir inputV =
  let inputV' = VS.concat $ map d2cdV inputV :: VS.Vector CDouble
      n = length inputV
      m = VS.length . head $ inputV
      mz= length . head $ z
      num' = d2cd num
      denom' = d2cd denom
      blen = length num'
      alen = length denom'
      z' = d2cd . concat $ z
      dir' = unsafePerformIO $ newCString dir
      (vv,zz) = filterXCore num' blen denom' alen z' dir' m n inputV' 
   in (flip mkChunksV m $ cd2dV vv, flip mkChunksL mz $ cd2d zz)


filterXCore :: [CDouble] ->  Int -> [CDouble] ->  Int -> [CDouble] ->  CString -> Int -> Int -> VS.Vector CDouble -> (VS.Vector CDouble, [CDouble])
filterXCore b blen a alen z dir m n input
  = unsafePerformIO $ VS.unsafeWith input $ \ptrInput ->
   withArray b $ \ptrb ->
   withArray a $ \ptra ->
   withArray z $ \ptrZin ->
   withArray (replicate ilen 0.0) $ \ptrOutput ->
   withArray (replicate zlen 0.0) $ \ptrZout ->
   do c'filter ptrOutput ptrZout ptrb wblen ptra walen ptrInput wm wn ptrZin dir
      newForeignPtr finalizerFree ptrOutput >>= \foreignptrOutput ->
        return $ ( VS.unsafeFromForeignPtr0 foreignptrOutput ilen
                 , unsafePerformIO $ peekArray zlen ptrZout
                 )
      where ilen = m * n
            wilen = itow32 ilen
            walen = itow32 alen
            wblen = itow32 blen
            wm    = itow32 m
            wn    = itow32 n
            zlen = ((max blen alen)-1) * n


calcInitCond :: ([Double],[Double]) -> [Double]
calcInitCond (num,denom) =
  let n = length num
      k = ident (n-1)
      kcv= toColumns k
      a = fromList (tail num)
      k' = fromColumns (a:(tail kcv))
      k0 = ((n-1)><(n-1)) (1:replicate ((n-1)*(n-1)-1) 0)
      k''= k' + k0
      y = zip (VS.toList (ND.flatten . trans $ k'')) [0,1..]
      y'= [if i `elem` [(n-1),2*n-1..(n-1)*(n-1)] then -1.0 else x |(x,i)<-y]
   in concat $ toLists $ trans $(trans $ ((n-1)><(n-1)) y') 
        <\> ((((n-1)><1) (tail denom)) - scale (head denom) (((n-1)><1) (tail num))) 


itow32 :: Int -> CUInt
itow32 = fromIntegral

d2cd :: [Double] -> [CDouble]
d2cd = map realToFrac

cd2d :: [CDouble] -> [Double]
cd2d = map realToFrac

d2cdV :: VS.Vector Double -> VS.Vector CDouble
d2cdV = VS.map realToFrac

cd2dV :: VS.Vector CDouble -> VS.Vector Double
cd2dV = VS.map realToFrac


foreign import ccall "filterX.h filter" c'filter :: Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CUInt -> Ptr CDouble -> CUInt -> Ptr CDouble -> CUInt -> CUInt -> Ptr CDouble -> CString -> IO()

