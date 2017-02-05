
{-# LANGUAGE ForeignFunctionInterface #-}

module HasKAL.SignalProcessingUtils.FilterX
  ( FilterDirection (..)
  , calcInitCond
  -- | IIR filter forsingle time-series vector data
  , iir0
  , iir
  , filtfilt0
  -- | FIR filter for single time-series vector data
  , fir0
  , fir
  , firfiltfilt0
  -- | IIR filter for multiple time-series vector data
  , iirM0
  , iirM
  , filtfiltM0
  -- | FIR filter for multiple time-series vector data
  , firM0
  , firM
  , firfiltfiltM0
  ) where

import qualified Data.Vector.Storable as VS (Vector, concat, drop, length, slice, unsafeWith, unsafeFromForeignPtr0,map, fromList, toList, unsafeToForeignPtr0)
import Data.Word
import Foreign.C.Types
import Foreign.C.String
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr_, newForeignPtr, withForeignPtr, mallocForeignPtrArray0, touchForeignPtr)
import Foreign.Ptr
import Foreign.Marshal.Alloc(finalizerFree, free)
import Foreign.Marshal.Array
import HasKAL.Misc.Function (mkChunksV,mkChunksL)
import Numeric.LinearAlgebra (flipud, fromBlocks, fromList, fromColumns, toColumns, fromRows, toRows, ident, scale, toLists, (><), (<\>), dropRows, rows, takeRows, asRow, tr)
import qualified Numeric.LinearAlgebra.Data as ND
import System.IO.Unsafe
import Control.DeepSeq (deepseq, NFData)


data FilterDirection = Reverse | Forward deriving (Show, Eq, Ord)

trans = tr

-- | filtfilt0 (num, denom) inputV
filtfilt0 :: ([Double], [Double]) -> VS.Vector Double -> VS.Vector Double
filtfilt0 (num, denom) inputV = head $ filtfiltM0 (num, denom) [inputV]


-- | iir (num, denom) z dir inputV
iir :: ([Double], [Double]) -> [Double] -> FilterDirection -> VS.Vector Double -> (VS.Vector Double,[Double])
iir (num, denom) z dir inputV = let a = iirM (num, denom) [z] dir [inputV]
                                       in (head . fst $ a, head . snd $ a)


-- | iir0 (num, denom) inputV
iir0 :: ([Double], [Double]) -> VS.Vector Double -> VS.Vector Double
iir0 (num, denom) inputV = head . fst $ iirM0 (num, denom) [inputV]


-- | fir num z dir inputV
fir :: [Double] -> [Double] -> FilterDirection -> VS.Vector Double -> (VS.Vector Double,[Double])
fir num z dir inputV = let a = iirM (num, denom) [z] dir [inputV]
                           denom = 1.0:replicate (length num-1) 0.0
                         in (head . fst $ a, head . snd $ a)


-- | fir0 num inputV
fir0 :: [Double] -> VS.Vector Double -> VS.Vector Double
fir0 num inputV = head . fst $ iirM0 (num, denom) [inputV]
  where denom = 1.0:replicate (length num-1) 0.0


firfiltfilt0 :: [Double] -> VS.Vector Double -> VS.Vector Double
firfiltfilt0 num inputV = head $ filtfiltM0 (num, denom) [inputV]
  where denom = 1.0:replicate (length num-1) 0.0


-- | filtfiltM0 (num, denom) inputV
filtfiltM0 :: ([Double], [Double]) -> [VS.Vector Double] -> [VS.Vector Double]
filtfiltM0 (num, denom) inputV =
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
      (dum,zi) = iirM (num, denom) (map VS.toList (toColumns (ic * takeRows 1 xi''))) Forward xi'
      -- Use the final conditions of the initial part for the actual signal:
      (ys,zs) = iirM (num, denom) zi Forward inputV -- "s"teady state
      (yf,zdum) = iirM (num, denom) zs Forward xf'  -- "f"inal conditions
      -- Filter signal again in reverse order:
      yEdge = asRow $ (fromColumns yf) ND.! (nEdge-1)
      (dum',zf) = iirM (num, denom) (map VS.toList (toColumns (ic * yEdge))) Reverse yf
   in fst $ iirM (num, denom) zf Reverse ys


-- | iirM0 (num, denom) inputV
iirM0 :: ([Double], [Double]) -> [VS.Vector Double] -> ([VS.Vector Double], [[Double]])
iirM0 (num, denom) inputV =
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
      (dum,zi) = iirM (num, denom) (map VS.toList (toColumns (ic * takeRows 1 xi''))) Forward xi'
      -- Use the final conditions of the initial part for the actual signal:
  in iirM (num, denom) zi Forward inputV


-- | multi vector input version
-- | iirM (num, denom) z dir inputV
iirM :: ([Double], [Double]) -> [[Double]] -> FilterDirection -> [VS.Vector Double] -> ([VS.Vector Double], [[Double]])
iirM (num, denom) z dir inputV = unsafePerformIO $ do
  let inputV' = VS.concat $ map d2cdV inputV :: VS.Vector CDouble
      n = length inputV
      m = VS.length . head $ inputV
      mz= length . head $ z
      num' = d2cd num
      denom' = d2cd denom
      blen = length num'
      alen = length denom'
      z' = d2cd . concat $ z
      dir' | dir==Forward = 1 :: Int
           | dir==Reverse = 0 :: Int
  let(vv,zz) = iirMCore num' blen denom' alen z' dir' m n inputV'
  return $(mkChunksV (cd2dV vv) 0 m, mkChunksL (cd2d zz) 0 mz)


-- | iirM0 (num, denom) inputV
iirVLM0 :: ([Double], [Double]) -> [VS.Vector Double] -> ([[Double]], [[Double]])
iirVLM0 (num, denom) inputV =
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
      (dum,zi) = iirM (num, denom) (map VS.toList (toColumns (ic * takeRows 1 xi''))) Forward xi'
      -- Use the final conditions of the initial part for the actual signal:
  in iirVLM (num, denom) zi Forward inputV


-- | multi vector input version
-- | iirM (num, denom) z dir inputV
iirVLM :: ([Double], [Double]) -> [[Double]] -> FilterDirection -> [VS.Vector Double] -> ([[Double]], [[Double]])
iirVLM (num, denom) z dir inputV = unsafePerformIO $ do
  let inputV' = VS.concat $ map d2cdV inputV :: VS.Vector CDouble
      n = length inputV
      m = VS.length . head $ inputV
      mz= length . head $ z
      num' = d2cd num
      denom' = d2cd denom
      blen = length num'
      alen = length denom'
      z' = d2cd . concat $ z
      dir' | dir==Forward = 1 :: Int
           | dir==Reverse = 0 :: Int
  let(ll,zz) = iirVLMCore num' blen denom' alen z' dir' m n inputV'
  return $(mkChunksL (cd2d ll) 0 m, mkChunksL (cd2d zz) 0 mz)


firM0 :: [Double] -> [VS.Vector Double] -> ([VS.Vector Double], [[Double]])
firM0 num inputV = iirM0 (num, denom) inputV
  where denom = 1.0:replicate (length num-1) 0.0


firM :: [Double] -> [[Double]] -> FilterDirection -> [VS.Vector Double] -> ([VS.Vector Double], [[Double]])
firM num z dir inputV = iirM (num, denom) z dir inputV
  where denom = 1.0:replicate (length num-1) 0.0


firfiltfiltM0 :: [Double] -> [VS.Vector Double] -> [VS.Vector Double]
firfiltfiltM0 num inputV = filtfiltM0 (num, denom) inputV
  where denom = 1.0:replicate (length num-1) 0.0


iirMCore :: [CDouble] ->  Int -> [CDouble] ->  Int -> [CDouble] ->  Int -> Int -> Int -> VS.Vector CDouble -> (VS.Vector CDouble, [CDouble])
iirMCore b blen a alen z d m n input
 = unsafePerformIO $ do
   let (fptrInput, ilen) = VS.unsafeToForeignPtr0 input
   withForeignPtr fptrInput $ \ptrInput ->
    withArray b $ \ptrb ->
    withArray a $ \ptra ->
    withArray z $ \ptrZin -> do
    mallocForeignPtrArray0 ilen >>= \fptrOutput -> withForeignPtr fptrOutput $ \ptrOutput ->
     mallocForeignPtrArray0 zlen >>= \fptrZout -> withForeignPtr fptrZout $ \ptrZout -> do
      c'filter ptrOutput ptrZout ptrb cblen ptra calen ptrInput cm cn ptrZin cd
      a <- return $ VS.unsafeFromForeignPtr0 fptrOutput ilen
      a `deepseq` return ()
      b <- peekArray zlen ptrZout
      b `deepseq` return ()
      return $ (a, b)
       where ilen = m * n
             calen = fromIntegral alen :: CInt
             cblen = fromIntegral blen :: CInt
             cd    = fromIntegral d    :: CInt
             cm    = fromIntegral m    :: CInt
             cn    = fromIntegral n    :: CInt
             zlen  = ((max blen alen)-1) * n


iirVLMCore :: [CDouble] ->  Int -> [CDouble] ->  Int -> [CDouble] ->  Int -> Int -> Int -> VS.Vector CDouble -> ([CDouble], [CDouble])
iirVLMCore b blen a alen z d m n input
 = unsafePerformIO $ do
   let (fptrInput, ilen) = VS.unsafeToForeignPtr0 input
   withForeignPtr fptrInput $ \ptrInput ->
    withArray b $ \ptrb ->
    withArray a $ \ptra ->
    withArray z $ \ptrZin -> do
    mallocForeignPtrArray0 ilen >>= \fptrOutput -> withForeignPtr fptrOutput $ \ptrOutput ->
     mallocForeignPtrArray0 zlen >>= \fptrZout -> withForeignPtr fptrZout $ \ptrZout -> do
      c'filter ptrOutput ptrZout ptrb cblen ptra calen ptrInput cm cn ptrZin cd
      a <- peekArray ilen ptrOutput
      a `deepseq` return ()
      b <- peekArray zlen ptrZout
      b `deepseq` return ()
      return $ (a, b)
       where ilen = m * n
             calen = fromIntegral alen :: CInt
             cblen = fromIntegral blen :: CInt
             cd    = fromIntegral d    :: CInt
             cm    = fromIntegral m    :: CInt
             cn    = fromIntegral n    :: CInt
             zlen  = ((max blen alen)-1) * n


iirLLMCore :: [CDouble] ->  Int -> [CDouble] ->  Int -> [CDouble] ->  Int -> Int -> Int -> [CDouble] -> ([CDouble], [CDouble])
iirLLMCore b blen a alen z d m n input
 = unsafePerformIO $ do
   withArray input $ \ptrInput ->
    withArray b $ \ptrb ->
    withArray a $ \ptra ->
    withArray z $ \ptrZin -> do
    mallocForeignPtrArray0 ilen >>= \fptrOutput -> withForeignPtr fptrOutput $ \ptrOutput ->
     mallocForeignPtrArray0 zlen >>= \fptrZout -> withForeignPtr fptrZout $ \ptrZout -> do
      c'filter ptrOutput ptrZout ptrb cblen ptra calen ptrInput cm cn ptrZin cd
      a <- peekArray ilen ptrOutput
      a `deepseq` return ()
      b <- peekArray zlen ptrZout
      b `deepseq` return ()
      return $ (a, b)
       where ilen = m * n
             calen = fromIntegral alen :: CInt
             cblen = fromIntegral blen :: CInt
             cd    = fromIntegral d    :: CInt
             cm    = fromIntegral m    :: CInt
             cn    = fromIntegral n    :: CInt
             zlen  = ((max blen alen)-1) * n


iirLVMCore :: [CDouble] ->  Int -> [CDouble] ->  Int -> [CDouble] ->  Int -> Int -> Int -> [CDouble] -> (VS.Vector CDouble, [CDouble])
iirLVMCore b blen a alen z d m n input
 = unsafePerformIO $ do
   withArray input $ \ptrInput ->
    withArray b $ \ptrb ->
    withArray a $ \ptra ->
    withArray z $ \ptrZin -> do
    mallocForeignPtrArray0 ilen >>= \fptrOutput -> withForeignPtr fptrOutput $ \ptrOutput ->
     mallocForeignPtrArray0 zlen >>= \fptrZout -> withForeignPtr fptrZout $ \ptrZout -> do
      c'filter ptrOutput ptrZout ptrb cblen ptra calen ptrInput cm cn ptrZin cd
      a <- return $ VS.unsafeFromForeignPtr0 fptrOutput ilen
      a `deepseq` return ()
      b <- peekArray zlen ptrZout
      b `deepseq` return ()
      return $ (a, b)
       where ilen = m * n
             calen = fromIntegral alen :: CInt
             cblen = fromIntegral blen :: CInt
             cd    = fromIntegral d    :: CInt
             cm    = fromIntegral m    :: CInt
             cn    = fromIntegral n    :: CInt
             zlen  = ((max blen alen)-1) * n



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


d2cd :: [Double] -> [CDouble]
d2cd = map realToFrac

cd2d :: [CDouble] -> [Double]
cd2d = map realToFrac

d2cdV :: VS.Vector Double -> VS.Vector CDouble
d2cdV = VS.map realToFrac

cd2dV :: VS.Vector CDouble -> VS.Vector Double
cd2dV = VS.map realToFrac


foreign import ccall "filterX.h filter" c'filter :: Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> CInt -> Ptr CDouble -> CInt -> IO()
