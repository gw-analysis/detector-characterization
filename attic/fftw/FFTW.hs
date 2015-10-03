


module FFTW
( dftRH1d
, dftHR1d
, dftRH2d
, dftHR2d
, dct1d
, idct1d
, dct2d
, idct2d
-- * Real to Complex
, dftRC1d
, dftCR1d
)
where

import qualified Data.Vector.Storable as VS
import Foreign.Storable (pokeElemOff)
import Data.Array.CArray
import Math.FFT
import System.IO.Unsafe (unsafePerformIO)
import qualified Numeric.LinearAlgebra as NL
import Numeric.LinearAlgebra.Devel (unsafeMatrixToForeignPtr, matrixFromVector, MatrixOrder(..))
import Foreign.ForeignPtr (withForeignPtr)


dftRH1d :: VS.Vector Double -> VS.Vector Double
dftRH1d vin = do
  let len = VS.length vin :: Int
      arr = unsafePerformIO $ createCArray (0, len-1)
        $ \ptr -> VS.zipWithM_ (pokeElemOff ptr) (VS.fromList [0..len-1]) vin
      (n,  ptr) = toForeignPtr $ dftRH arr
   in VS.unsafeFromForeignPtr0 ptr n


dftHR1d :: VS.Vector Double -> VS.Vector Double
dftHR1d vin = do
  let len = VS.length vin :: Int
      arr = unsafePerformIO $ createCArray (0, len-1)
        $ \ptr -> VS.zipWithM_ (pokeElemOff ptr) (VS.fromList [0..len-1]) vin
      (n,  ptr) = toForeignPtr $ dftHR arr
   in VS.map (1/fromIntegral len *) $ VS.unsafeFromForeignPtr0 ptr n


dftRH2d :: NL.Matrix Double -> NL.Matrix Double
dftRH2d m = do
    let mm = NL.rows m :: Int
        mn = NL.cols m :: Int
        arr = unsafePerformIO
          $ unsafeForeignPtrToCArray (fst $ unsafeMatrixToForeignPtr m) ((0, 0), (mm-1, mn-1))
        (n,  ptr) = toForeignPtr $  dftRHN [0, 1] arr
     in matrixFromVector RowMajor mm mn $ VS.unsafeFromForeignPtr0 ptr n


dftHR2d :: NL.Matrix Double -> NL.Matrix Double
dftHR2d m = do
    let mm = NL.rows m :: Int
        mn = NL.cols m :: Int
        arr = unsafePerformIO
          $ unsafeForeignPtrToCArray (fst $ unsafeMatrixToForeignPtr m) ((0, 0), (mm-1, mn-1))
        (n, ptr) = toForeignPtr $ dftHRN [0, 1] arr
     in matrixFromVector RowMajor mm mn $ VS.map (scaling2d mm mn * ) $ VS.unsafeFromForeignPtr0 ptr n
          where
            scaling2d m n = 1/(fromIntegral m * fromIntegral n)


dct1d :: VS.Vector Double -> VS.Vector Double
dct1d vin = do
  let len = VS.length vin :: Int
      arr = unsafePerformIO $ createCArray (0, len-1)
        $ \ptr -> VS.zipWithM_ (pokeElemOff ptr) (VS.fromList [0..len-1]) vin
      (n,  ptr) = toForeignPtr $ dct2 arr
   in NL.mapVectorWithIndex (\i v -> scaling i len v) $ VS.unsafeFromForeignPtr0 ptr n


idct1d :: VS.Vector Double -> VS.Vector Double
idct1d vin = do
  let len = VS.length vin :: Int
      vin' = NL.mapVectorWithIndex (\i v -> rescaling i len v) vin
      arr = unsafePerformIO $ createCArray (0, len-1)
        $ \ptr -> VS.zipWithM_ (pokeElemOff ptr) (VS.fromList [0..len-1]) vin'
      (n,  ptr) = toForeignPtr $ dct3 arr
   in VS.unsafeFromForeignPtr0 ptr n


dct2d :: NL.Matrix Double -> NL.Matrix Double
dct2d m = do
    let mm = NL.rows m :: Int
        mn = NL.cols m :: Int
        arr = unsafePerformIO
          $ unsafeForeignPtrToCArray (fst $ unsafeMatrixToForeignPtr m) ((0, 0), (mm-1, mn-1))
        (n,  ptr) = toForeignPtr $ dct2N [0, 1] arr
        outM = matrixFromVector RowMajor mm mn
          $ VS.map (scaling2d mm mn * ) $ VS.unsafeFromForeignPtr0 ptr n

     in NL.mapMatrixWithIndex (\(r, c) v -> ar r c v) outM


idct2d :: NL.Matrix Double -> NL.Matrix Double
idct2d m = do
    let mm = NL.rows m :: Int
        mn = NL.cols m :: Int
        m' = NL.mapMatrixWithIndex (\(r, c) v -> ari r c v) m
        arr = unsafePerformIO
          $ unsafeForeignPtrToCArray (fst $ unsafeMatrixToForeignPtr m') ((0, 0), (mm-1, mn-1))
        (n, ptr) = toForeignPtr $ dct3N [0, 1] arr
     in matrixFromVector RowMajor mm mn $ VS.map (scaling2d mm mn * ) $ VS.unsafeFromForeignPtr0 ptr n

dftRC1d :: VS.Vector Double -> VS.Vector (NL.Complex Double)
dftRC1d vin = do
  let len = VS.length vin :: Int
      arr = unsafePerformIO $ createCArray (0, len-1)
        $ \ptr -> VS.zipWithM_ (pokeElemOff ptr) (VS.fromList [0..len-1]) vin
      (n,  ptr) = toForeignPtr $ dftRC arr
   in VS.unsafeFromForeignPtr0 ptr n


dftCR1d :: VS.Vector (NL.Complex Double) -> VS.Vector Double
dftCR1d vin = do
  let len = VS.length vin :: Int
      arr = unsafePerformIO $ createCArray (0, len-1)
        $ \ptr -> VS.zipWithM_ (pokeElemOff ptr) (VS.fromList [0..len-1]) vin
      (n,  ptr) = toForeignPtr $ dftCR arr
   -- in VS.map (1/fromIntegral len *) $ VS.unsafeFromForeignPtr0 ptr n
   in VS.unsafeFromForeignPtr0 ptr n -- 1/N無しで元に戻る定義になっている


scaling i n v = do
-- | fftw related
  let fftwscale = 0.5 :: Double
-- | normalization factor
      normScale = sqrt (2.0/fromIntegral n)
-- \ scale factor
  case i of
    0 -> fftwscale*normScale/sqrt 2.0*v
    _ -> fftwscale*normScale*v


rescaling i n v = do
-- | fftw related
  let fftwscale = 0.5 :: Double
-- | normalization factor
      normScale = sqrt (2.0/fromIntegral n)
-- \ scale factor
  case i of
    0 -> fftwscale*normScale*sqrt 2.0 * v
    _ -> fftwscale*normScale*v


scaling2d m n = do
-- | fftw related
  let fftwscale = 0.5*0.5 :: Double
-- | normalization factor
      normScale = sqrt (4.0/(fromIntegral m * fromIntegral n))
-- \ scale factor
   in fftwscale*normScale


ar r c v
  | r==0&&c==0 = 0.5 * v
  | r==0&&c/=0 = v / sqrt 2
  | r/=0&&c==0 = v / sqrt 2
  | r/=0&&c/=0 = v


ari r c v
  | r==0&&c==0 = 2 * v
  | r==0&&c/=0 = v * sqrt 2
  | r/=0&&c==0 = v * sqrt 2
  | r/=0&&c/=0 = v

