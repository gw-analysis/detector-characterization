
{-# LANGUAGE ForeignFunctionInterface #-}

module HasKAL.SignalProcessingUtils.Filter
  ( iir
  , iirW
  , fir
--  , fir'
  , iirFilter
  , firFilter
  , filtfilt
  , sosfilter
  , sos1filter
  , firInit
  , iirInit
  , firFilterInit
  , iirFilterInit
  , filtfiltInit
  , sosfilterInit
  , sos1filterInit
  , firFiltfiltV
  , firFiltfiltVInit
  , firFiltfilt
  , firFiltfiltInit
  , sosfiltfilt
  , sosfiltfiltInit
  , sos1filtfilt
  , sos1filtfiltInit
  , calcInitCond
  ) where

import qualified Data.Vector.Storable as VS (Vector, length, unsafeWith, unsafeFromForeignPtr0,map)
import Data.Word
import Foreign.C.Types
-- import Foreign.C.String
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr_)
import Foreign.Ptr
import Foreign.Marshal.Array
import Numeric.LinearAlgebra (fromBlocks, fromList, fromRows, ident, scale, toLists, toRows, (><), (<\>))
import System.IO.Unsafe
import Unsafe.Coerce (unsafeCoerce)

import HasKAL.WaveUtils.Data (WaveData(..), mkWaveData)

iirW :: ([Double],[Double]) -> WaveData -> WaveData
iirW coeffs w = w'
  where w' = mkWaveData (detector w) (dataType w) (samplingFrequency w) (startGPSTime w) (stopGPSTime w) v
        v = iir coeffs (gwdata w)

iir :: ([Double],[Double]) -> VS.Vector Double -> VS.Vector Double
iir (numCoeff, denomCoeff) inputV = do
  let ilen = VS.length inputV
      flen = length numCoeff
      inputV' = d2cdV inputV :: VS.Vector CDouble
      numCoeff' = d2cd numCoeff
      denomCoeff' = d2cd denomCoeff
  cd2dV $ iirCore inputV' ilen numCoeff' denomCoeff' flen


iirInit :: ([Double],[Double]) -> [Double] -> VS.Vector Double -> VS.Vector Double
iirInit (numCoeff, denomCoeff) initCoeff inputV = do
  let ilen = VS.length inputV
      flen = length numCoeff
      inputV' = d2cdV inputV :: VS.Vector CDouble
      numCoeff' = d2cd numCoeff
      denomCoeff' = d2cd denomCoeff
      initCoeff' = d2cd initCoeff
  cd2dV $ iir_initCore inputV' ilen numCoeff' denomCoeff' initCoeff' flen


iirFilter :: [Double] -> [Double] -> [Double] -> [Double]
iirFilter input numCoeff denomCoeff = do
  let ilen = length input :: Int
      flen = length numCoeff
      input' = d2cd input
      numCoeff' = d2cd numCoeff
      denomCoeff' = d2cd denomCoeff
  cd2d $ iirFilterCore input' ilen numCoeff' denomCoeff' flen


iirFilterInit :: [Double] -> [Double] -> [Double] -> [Double] -> [Double]
iirFilterInit input numCoeff denomCoeff initCoeff = do
  let ilen = length input :: Int
      flen = length numCoeff
      input' = d2cd input
      numCoeff' = d2cd numCoeff
      denomCoeff' = d2cd denomCoeff
      initCoeff' = d2cd initCoeff
  cd2d $ iirFilterInitCore input' ilen numCoeff' denomCoeff' flen initCoeff'


fir :: [Double] -> VS.Vector Double -> VS.Vector Double
fir firCoeff inputV = do
  let ilen = VS.length inputV
      flen = length firCoeff
      inputV' = d2cdV inputV :: VS.Vector CDouble
      firCoeff' = d2cd firCoeff
  cd2dV $ firCore inputV' ilen firCoeff' flen


firInit :: [Double] -> [Double] -> VS.Vector Double -> VS.Vector Double
firInit firCoeff initCoeff inputV = do
  let ilen = VS.length inputV
      flen = length firCoeff
      inputV' = d2cdV inputV :: VS.Vector CDouble
      firCoeff' = d2cd firCoeff
      initCoeff'= d2cd initCoeff
  cd2dV $ fir_initCore inputV' ilen firCoeff' initCoeff' flen


firFiltfiltV :: [Double] -> VS.Vector Double -> VS.Vector Double
firFiltfiltV firCoeff inputV = do
  let ilen = VS.length inputV
      flen = length firCoeff
      inputV' = d2cdV inputV :: VS.Vector CDouble
      firCoeff' = d2cd firCoeff
  cd2dV $ firFiltfiltVCore inputV' ilen firCoeff' flen


firFiltfiltVInit :: [Double] -> [Double] -> VS.Vector Double -> VS.Vector Double
firFiltfiltVInit firCoeff initCoeff inputV = do
  let ilen = VS.length inputV
      flen = length firCoeff
      inputV' = d2cdV inputV :: VS.Vector CDouble
      firCoeff' = d2cd firCoeff
      initCoeff'= d2cd initCoeff
  cd2dV $ firFiltfiltV_initCore inputV' ilen firCoeff' initCoeff' flen


fir' :: [Double] -> VS.Vector Double -> VS.Vector Double
fir' firCoeff inputV = do
  let ilen = VS.length inputV
      flen = length firCoeff
--      inputV' = d2cdV inputV :: VS.Vector CDouble
--      firCoeff' = d2cd firCoeff
--  cd2dV $ firCore inputV' ilen firCoeff' flen
  firCore' inputV ilen firCoeff flen


firFilter :: [Double] -> [Double] -> [Double]
firFilter input firCoeff = do
  let ilen = length input :: Int
      flen = length firCoeff
      input' = d2cd input
      firCoeff' = d2cd firCoeff
  cd2d $ firFilterCore input' ilen firCoeff' flen


firFilterInit :: [Double] -> [Double] -> [Double]-> [Double]
firFilterInit input firCoeff initCoeff = do
  let ilen = length input :: Int
      flen = length firCoeff
      input' = d2cd input
      firCoeff' = d2cd firCoeff
      initCoeff' = d2cd initCoeff
  cd2d $ firFilterInitCore input' ilen firCoeff' initCoeff' flen


firFiltfilt :: [Double] -> [Double] -> [Double]
firFiltfilt input firCoeff = do
  let ilen = length input :: Int
      flen = length firCoeff
      input' = d2cd input
      firCoeff' = d2cd firCoeff
  cd2d $ firFiltfiltCore input' ilen firCoeff' flen


firFiltfiltInit :: [Double] -> [Double] -> [Double]-> [Double]
firFiltfiltInit input firCoeff initCoeff = do
  let ilen = length input :: Int
      flen = length firCoeff
      input' = d2cd input
      firCoeff' = d2cd firCoeff
      initCoeff' = d2cd initCoeff
  cd2d $ firFiltfiltInitCore input' ilen firCoeff' initCoeff' flen


filtfilt :: ([Double],[Double]) -> VS.Vector Double -> VS.Vector Double
filtfilt (numCoeff, denomCoeff) inputV = do
  let ilen = VS.length inputV
      flen = length numCoeff
      inputV' = d2cdV inputV :: VS.Vector CDouble
      numCoeff' = d2cd numCoeff
      denomCoeff' = d2cd denomCoeff
  cd2dV $ filtfiltCore inputV' ilen numCoeff' denomCoeff' flen


filtfiltInit :: ([Double],[Double]) -> [Double] -> VS.Vector Double -> VS.Vector Double
filtfiltInit (numCoeff, denomCoeff) initCoeff inputV = do
  let ilen = VS.length inputV
      flen = length numCoeff
      inputV' = d2cdV inputV :: VS.Vector CDouble
      numCoeff' = d2cd numCoeff
      denomCoeff' = d2cd denomCoeff
      initCoeff' = d2cd initCoeff
  cd2dV $ filtfilt_initCore inputV' ilen numCoeff' denomCoeff' initCoeff' flen


sosfilter :: [([Double], [Double])] -> VS.Vector Double -> VS.Vector Double
sosfilter coeffs inputV =
  let (num, denom) = unzip coeffs
      nums = map (\i->d2cd $ map (!!i) num) [0..length (head num)-1]
      denoms = map (\i->d2cd $ map (!!i) denom) [0..length (head denom)-1]
      ilen = VS.length inputV
      nsec = length coeffs
      inputV' = d2cdV inputV :: VS.Vector CDouble
   in cd2dV $ sosfilterCore inputV' ilen (nums!!0) (nums!!1) (nums!!2) (denoms!!0) (denoms!!1) (denoms!!2) nsec


sosfilterInit :: [([Double], [Double])] -> [[Double]] -> VS.Vector Double -> VS.Vector Double
sosfilterInit coeffs initCoeff inputV =
  let (num, denom) = unzip coeffs
      nums = map (\i->d2cd $ map (!!i) num) [0..length (head num)-1]
      nums0 = map head nums
      nums1 = map (!!1) nums
      nums2 = map (!!2) nums
      denoms = map (\i->d2cd $ map (!!i) denom) [0..length (head denom)-1]
      denoms0 = map head denoms
      denoms1 = map (!!1) denoms
      denoms2 = map (!!2) denoms
      initCoeffs = map (\i->d2cd $ map (!!i) initCoeff) [0..length (head initCoeff)-1]
      initCoeffs1 = map head initCoeffs
      initCoeffs2 = map (!!1) initCoeffs
      ilen = VS.length inputV
      nsec = length coeffs
      inputV' = d2cdV inputV :: VS.Vector CDouble
   in cd2dV $ sosfilter_initCore inputV' ilen nums0 nums1 nums2 denoms0 denoms1 denoms2 nsec initCoeffs1 initCoeffs2


sosfiltfilt :: [([Double], [Double])] -> VS.Vector Double -> VS.Vector Double
sosfiltfilt coeffs inputV =
  let (num, denom) = unzip coeffs
      nums = map (\i->d2cd $ map (!!i) num) [0..length (head num)-1]
      denoms = map (\i->d2cd $ map (!!i) denom) [0..length (head denom)-1]
      ilen = VS.length inputV
      nsec = length coeffs
      inputV' = d2cdV inputV :: VS.Vector CDouble
   in cd2dV $ sosfiltfiltCore inputV' ilen (nums!!0) (nums!!1) (nums!!2) (denoms!!0) (denoms!!1) (denoms!!2) nsec


sosfiltfiltInit :: [([Double], [Double])] -> [[Double]] -> VS.Vector Double -> VS.Vector Double
sosfiltfiltInit coeffs initCoeff inputV =
  let (num, denom) = unzip coeffs
      nums = map (\i->d2cd $ map (!!i) num) [0..length (head num)-1]
      nums0 = map head nums
      nums1 = map (!!1) nums
      nums2 = map (!!2) nums
      denoms = map (\i->d2cd $ map (!!i) denom) [0..length (head denom)-1]
      denoms0 = map head denoms
      denoms1 = map (!!1) denoms
      denoms2 = map (!!2) denoms
      initCoeffs = map (\i->d2cd $ map (!!i) initCoeff) [0..length (head initCoeff)-1]
      initCoeffs1 = map head initCoeffs
      initCoeffs2 = map (!!1) initCoeffs
      ilen = VS.length inputV
      nsec = length coeffs
      inputV' = d2cdV inputV :: VS.Vector CDouble
   in cd2dV $ sosfiltfilt_initCore inputV' ilen nums0 nums1 nums2 denoms0 denoms1 denoms2 nsec initCoeffs1 initCoeffs2


sos1filter :: ([Double], [Double]) -> VS.Vector Double -> VS.Vector Double
sos1filter (num, denom) inputV =
  let inputV' = d2cdV inputV :: VS.Vector CDouble
      ilen = VS.length inputV
      num' = d2cd num
      denom' = d2cd denom
      nsec = 1
   in cd2dV $ sosfilterCore inputV' ilen [num'!!0] [num'!!1] [num'!!2] [denom'!!0] [denom'!!1] [denom'!!2] nsec


sos1filterInit :: ([Double], [Double]) -> (Double, Double) -> VS.Vector Double -> VS.Vector Double
sos1filterInit (num, denom) (init1, init2) inputV =
  let inputV' = d2cdV inputV :: VS.Vector CDouble
      ilen = VS.length inputV
      num' = d2cd num
      denom' = d2cd denom
      init1' = realToFrac init1
      init2' = realToFrac init2
      nsec = 1
   in cd2dV $ sosfilter_initCore inputV' ilen [num'!!0] [num'!!1] [num'!!2] [denom'!!0] [denom'!!1] [denom'!!2] nsec [init1'] [init2']


sos1filtfilt :: ([Double], [Double]) -> VS.Vector Double -> VS.Vector Double
sos1filtfilt (num, denom) inputV =
  let inputV' = d2cdV inputV :: VS.Vector CDouble
      ilen = VS.length inputV
      num' = d2cd num
      denom' = d2cd denom
      nsec = 1
   in cd2dV $ sosfiltfiltCore inputV' ilen [num'!!0] [num'!!1] [num'!!2] [denom'!!0] [denom'!!1] [denom'!!2] nsec


sos1filtfiltInit :: ([Double], [Double]) -> (Double, Double) -> VS.Vector Double -> VS.Vector Double
sos1filtfiltInit (num, denom) (init1, init2) inputV =
  let inputV' = d2cdV inputV :: VS.Vector CDouble
      ilen = VS.length inputV
      num' = d2cd num
      denom' = d2cd denom
      init1' = realToFrac init1
      init2' = realToFrac init2
      nsec = 1
   in cd2dV $ sosfiltfilt_initCore inputV' ilen [num'!!0] [num'!!1] [num'!!2] [denom'!!0] [denom'!!1] [denom'!!2] nsec [init1'] [init2']


-------------  Internal Functions  -----------------------------

cumsum :: [Double] -> [Double]
cumsum = scanl1 (+)


iirCore :: VS.Vector CDouble -> Int -> [CDouble] -> [CDouble] -> Int -> VS.Vector CDouble
iirCore input ilen numCoeff denomCoeff flen
  = unsafePerformIO $ VS.unsafeWith input $ \ptrInput ->
   withArray numCoeff $ \ptrNumCoeff ->
   withArray denomCoeff $ \ptrDenomCoeff ->
   allocaArray ilen $ \ptrOutput ->
   do c_iir_filter ptrInput wilen ptrNumCoeff ptrDenomCoeff wflen ptrOutput
      newForeignPtr_ ptrOutput >>= \foreignptrOutput ->
        return $ VS.unsafeFromForeignPtr0 foreignptrOutput ilen
      where wilen = itow32 ilen
            wflen = itow32 flen


iir_initCore :: VS.Vector CDouble -> Int -> [CDouble] -> [CDouble] -> [CDouble] -> Int -> VS.Vector CDouble
iir_initCore input ilen numCoeff denomCoeff initCoeff flen
  = unsafePerformIO $ VS.unsafeWith input $ \ptrInput ->
   withArray numCoeff $ \ptrNumCoeff ->
   withArray denomCoeff $ \ptrDenomCoeff ->
   withArray initCoeff $ \ptrInitCoeff ->
   allocaArray ilen $ \ptrOutput ->
   do c_iir_filter_init ptrInput wilen ptrNumCoeff ptrDenomCoeff ptrInitCoeff wflen ptrOutput
      newForeignPtr_ ptrOutput >>= \foreignptrOutput ->
        return $ VS.unsafeFromForeignPtr0 foreignptrOutput ilen
      where wilen = itow32 ilen
            wflen = itow32 flen


fir_initCore :: VS.Vector CDouble -> Int -> [CDouble] -> [CDouble] -> Int -> VS.Vector CDouble
fir_initCore input ilen firCoeff initCoeff flen
  = unsafePerformIO $ VS.unsafeWith input $ \ptrInput ->
   withArray firCoeff $ \ptrFirCoeff ->
   withArray initCoeff $ \ptrInitCoeff ->
   allocaArray ilen $ \ptrOutput ->
   do c_fir_filter_init ptrInput wilen ptrFirCoeff ptrInitCoeff wflen ptrOutput
      newForeignPtr_ ptrOutput >>= \foreignptrOutput ->
        return $ VS.unsafeFromForeignPtr0 foreignptrOutput ilen
      where wilen = itow32 ilen
            wflen = itow32 flen


firFiltfiltV_initCore :: VS.Vector CDouble -> Int -> [CDouble] -> [CDouble] -> Int -> VS.Vector CDouble
firFiltfiltV_initCore input ilen firCoeff initCoeff flen
  = unsafePerformIO $ VS.unsafeWith input $ \ptrInput ->
   withArray firCoeff $ \ptrFirCoeff ->
   withArray initCoeff $ \ptrInitCoeff ->
   allocaArray ilen $ \ptrOutput ->
   do c_fir_filtfilt_init ptrInput wilen ptrFirCoeff ptrInitCoeff wflen ptrOutput
      newForeignPtr_ ptrOutput >>= \foreignptrOutput ->
        return $ VS.unsafeFromForeignPtr0 foreignptrOutput ilen
      where wilen = itow32 ilen
            wflen = itow32 flen


firCore :: VS.Vector CDouble -> Int -> [CDouble] -> Int -> VS.Vector CDouble
firCore input ilen firCoeff flen
  = unsafePerformIO $ VS.unsafeWith input $ \ptrInput ->
   withArray firCoeff $ \ptrFirCoeff ->
   allocaArray ilen $ \ptrOutput ->
   do c_fir_filter ptrInput wilen ptrFirCoeff wflen ptrOutput
      newForeignPtr_ ptrOutput >>= \foreignptrOutput ->
        return $ VS.unsafeFromForeignPtr0 foreignptrOutput ilen
      where wilen = itow32 ilen
            wflen = itow32 flen


firFiltfiltVCore :: VS.Vector CDouble -> Int -> [CDouble] -> Int -> VS.Vector CDouble
firFiltfiltVCore input ilen firCoeff flen
  = unsafePerformIO $ VS.unsafeWith input $ \ptrInput ->
   withArray firCoeff $ \ptrFirCoeff ->
   allocaArray ilen $ \ptrOutput ->
   do c_fir_filtfilt ptrInput wilen ptrFirCoeff wflen ptrOutput
      newForeignPtr_ ptrOutput >>= \foreignptrOutput ->
        return $ VS.unsafeFromForeignPtr0 foreignptrOutput ilen
      where wilen = itow32 ilen
            wflen = itow32 flen


firCore' :: VS.Vector Double -> Int -> [Double] -> Int -> VS.Vector Double
firCore' input ilen firCoeff flen
  = unsafePerformIO $ VS.unsafeWith input $ \ptrInput ->
   withArray firCoeff $ \ptrFirCoeff ->
   allocaArray ilen $ \ptrOutput ->
   do c_fir_filter (castPtr ptrInput) wilen (castPtr ptrFirCoeff) wflen ptrOutput
      newForeignPtr_ (castPtr ptrOutput) >>= \foreignptrOutput ->
        return $ VS.unsafeFromForeignPtr0 foreignptrOutput ilen
      where wilen = itow32 ilen
            wflen = itow32 flen


iirFilterCore :: [CDouble] -> Int -> [CDouble] -> [CDouble] -> Int -> [CDouble]
iirFilterCore input ilen numCoeff denomCoeff flen
  = unsafePerformIO $ withArray input $ \ptrInput ->
   withArray numCoeff $ \ptrNumCoeff ->
   withArray denomCoeff $ \ptrDenomCoeff ->
   allocaArray ilen $ \ptrOutput ->
   do c_iir_filter ptrInput wilen ptrNumCoeff ptrDenomCoeff wflen ptrOutput
      peekArray ilen ptrOutput
      where wilen = itow32 ilen
            wflen = itow32 flen


iirFilterInitCore :: [CDouble] -> Int -> [CDouble] -> [CDouble] -> Int -> [CDouble] -> [CDouble]
iirFilterInitCore input ilen numCoeff denomCoeff flen initCoeff
  = unsafePerformIO $ withArray input $ \ptrInput ->
   withArray numCoeff $ \ptrNumCoeff ->
   withArray denomCoeff $ \ptrDenomCoeff ->
   withArray initCoeff $ \ptrInitCoeff ->
   allocaArray ilen $ \ptrOutput ->
   do c_iir_filter_core ptrInput wilen ptrNumCoeff ptrDenomCoeff wflen ptrInitCoeff ptrOutput
      peekArray ilen ptrOutput
      where wilen = itow32 ilen
            wflen = itow32 flen


firFilterInitCore :: [CDouble] -> Int -> [CDouble] -> [CDouble] -> Int -> [CDouble]
firFilterInitCore input ilen firCoeff initCoeff flen
  = unsafePerformIO $ withArray input $ \ptrInput ->
   withArray firCoeff $ \ptrFirCoeff ->
   withArray initCoeff $ \ptrInitCoeff -> 
   allocaArray ilen $ \ptrOutput ->
   do c_fir_filter_init ptrInput wilen ptrFirCoeff ptrInitCoeff wflen ptrOutput
      peekArray ilen ptrOutput
      where wilen = itow32 ilen
            wflen = itow32 flen


firFiltfiltInitCore :: [CDouble] -> Int -> [CDouble] -> [CDouble] -> Int -> [CDouble]
firFiltfiltInitCore input ilen firCoeff initCoeff flen
  = unsafePerformIO $ withArray input $ \ptrInput ->
   withArray firCoeff $ \ptrFirCoeff ->
   withArray initCoeff $ \ptrInitCoeff -> 
   allocaArray ilen $ \ptrOutput ->
   do c_fir_filtfilt_init ptrInput wilen ptrFirCoeff ptrInitCoeff wflen ptrOutput
      peekArray ilen ptrOutput
      where wilen = itow32 ilen
            wflen = itow32 flen


firFilterCore :: [CDouble] -> Int -> [CDouble] -> Int -> [CDouble]
firFilterCore input ilen firCoeff flen
  = unsafePerformIO $ withArray input $ \ptrInput ->
   withArray firCoeff $ \ptrFirCoeff ->
   allocaArray ilen $ \ptrOutput ->
   do c_fir_filter ptrInput wilen ptrFirCoeff wflen ptrOutput
      peekArray ilen ptrOutput
      where wilen = itow32 ilen
            wflen = itow32 flen


firFiltfiltCore :: [CDouble] -> Int -> [CDouble] -> Int -> [CDouble]
firFiltfiltCore input ilen firCoeff flen
  = unsafePerformIO $ withArray input $ \ptrInput ->
   withArray firCoeff $ \ptrFirCoeff ->
   allocaArray ilen $ \ptrOutput ->
   do c_fir_filtfilt ptrInput wilen ptrFirCoeff wflen ptrOutput
      peekArray ilen ptrOutput
      where wilen = itow32 ilen
            wflen = itow32 flen


filtfiltCore :: VS.Vector CDouble -> Int -> [CDouble] -> [CDouble] -> Int -> VS.Vector CDouble
filtfiltCore input ilen numCoeff denomCoeff flen
  = unsafePerformIO $ VS.unsafeWith input $ \ptrInput ->
   withArray numCoeff $ \ptrNumCoeff ->
   withArray denomCoeff $ \ptrDenomCoeff ->
   allocaArray ilen $ \ptrOutput ->
   do c_filtfilt ptrInput wilen ptrNumCoeff ptrDenomCoeff wflen ptrOutput
      newForeignPtr_ ptrOutput >>= \foreignptrOutput ->
        return $ VS.unsafeFromForeignPtr0 foreignptrOutput ilen
      where wilen = itow32 ilen
            wflen = itow32 flen


filtfilt_initCore :: VS.Vector CDouble -> Int -> [CDouble] -> [CDouble] -> [CDouble] -> Int -> VS.Vector CDouble
filtfilt_initCore input ilen numCoeff denomCoeff initCoeff flen
  = unsafePerformIO $ VS.unsafeWith input $ \ptrInput ->
   withArray numCoeff $ \ptrNumCoeff ->
   withArray denomCoeff $ \ptrDenomCoeff ->
   withArray initCoeff $ \ptrInitCoeff ->
   allocaArray ilen $ \ptrOutput ->
   do c_filtfilt_init ptrInput wilen ptrNumCoeff ptrDenomCoeff ptrInitCoeff wflen ptrOutput
      newForeignPtr_ ptrOutput >>= \foreignptrOutput ->
        return $ VS.unsafeFromForeignPtr0 foreignptrOutput ilen
      where wilen = itow32 ilen
            wflen = itow32 flen


sosfilterCore :: VS.Vector CDouble -> Int -> [CDouble] -> [CDouble] -> [CDouble] -> [CDouble] -> [CDouble] -> [CDouble] -> Int -> VS.Vector CDouble
sosfilterCore input ilen num0 num1 num2 denom0 denom1 denom2 nsec
  = unsafePerformIO $ VS.unsafeWith input $ \ptrInput ->
   withArray num0 $ \ptrNum0 ->
   withArray num1 $ \ptrNum1 ->
   withArray num2 $ \ptrNum2 ->
   withArray denom0 $ \ptrDenom0 ->
   withArray denom1 $ \ptrDenom1 ->
   withArray denom2 $ \ptrDenom2 ->
   allocaArray ilen $ \ptrOutput ->
   do c'sosfilter ptrInput wilen ptrNum0 ptrNum1 ptrNum2 ptrDenom0 ptrDenom1 ptrDenom2 wnsec ptrOutput
      newForeignPtr_ ptrOutput >>= \foreignptrOutput ->
        return $ VS.unsafeFromForeignPtr0 foreignptrOutput ilen
      where wilen = itow32 ilen
            wnsec = itow32 nsec


sosfiltfiltCore :: VS.Vector CDouble -> Int -> [CDouble] -> [CDouble] -> [CDouble] -> [CDouble] -> [CDouble] -> [CDouble] -> Int -> VS.Vector CDouble
sosfiltfiltCore input ilen num0 num1 num2 denom0 denom1 denom2 nsec
  = unsafePerformIO $ VS.unsafeWith input $ \ptrInput ->
   withArray num0 $ \ptrNum0 ->
   withArray num1 $ \ptrNum1 ->
   withArray num2 $ \ptrNum2 ->
   withArray denom0 $ \ptrDenom0 ->
   withArray denom1 $ \ptrDenom1 ->
   withArray denom2 $ \ptrDenom2 ->
   allocaArray ilen $ \ptrOutput ->
   do c'sosfiltfilt ptrInput wilen ptrNum0 ptrNum1 ptrNum2 ptrDenom0 ptrDenom1 ptrDenom2 wnsec ptrOutput
      newForeignPtr_ ptrOutput >>= \foreignptrOutput ->
        return $ VS.unsafeFromForeignPtr0 foreignptrOutput ilen
      where wilen = itow32 ilen
            wnsec = itow32 nsec


sosfilter_initCore :: VS.Vector CDouble -> Int -> [CDouble] -> [CDouble] -> [CDouble] -> [CDouble] -> [CDouble] -> [CDouble] -> Int -> [CDouble] -> [CDouble] -> VS.Vector CDouble
sosfilter_initCore input ilen num0 num1 num2 denom0 denom1 denom2 nsec init1 init2
  = unsafePerformIO $ VS.unsafeWith input $ \ptrInput ->
   withArray num0 $ \ptrNum0 ->
   withArray num1 $ \ptrNum1 ->
   withArray num2 $ \ptrNum2 ->
   withArray denom0 $ \ptrDenom0 ->
   withArray denom1 $ \ptrDenom1 ->
   withArray denom2 $ \ptrDenom2 ->
   withArray init1 $ \ptrInit1 ->
   withArray init2 $ \ptrInit2 ->
   allocaArray ilen $ \ptrOutput ->
   do c'sosfilter_init ptrInput wilen ptrNum0 ptrNum1 ptrNum2 ptrDenom0 ptrDenom1 ptrDenom2 wnsec ptrInit1 ptrInit2 ptrOutput
      newForeignPtr_ ptrOutput >>= \foreignptrOutput ->
        return $ VS.unsafeFromForeignPtr0 foreignptrOutput ilen
      where wilen = itow32 ilen
            wnsec = itow32 nsec


sosfiltfilt_initCore :: VS.Vector CDouble -> Int -> [CDouble] -> [CDouble] -> [CDouble] -> [CDouble] -> [CDouble] -> [CDouble] -> Int -> [CDouble] -> [CDouble] -> VS.Vector CDouble
sosfiltfilt_initCore input ilen num0 num1 num2 denom0 denom1 denom2 nsec init1 init2
  = unsafePerformIO $ VS.unsafeWith input $ \ptrInput ->
   withArray num0 $ \ptrNum0 ->
   withArray num1 $ \ptrNum1 ->
   withArray num2 $ \ptrNum2 ->
   withArray denom0 $ \ptrDenom0 ->
   withArray denom1 $ \ptrDenom1 ->
   withArray denom2 $ \ptrDenom2 ->
   withArray init1 $ \ptrInit1 ->
   withArray init2 $ \ptrInit2 ->
   allocaArray ilen $ \ptrOutput ->
   do c'sosfiltfilt_init ptrInput wilen ptrNum0 ptrNum1 ptrNum2 ptrDenom0 ptrDenom1 ptrDenom2 wnsec ptrInit1 ptrInit2 ptrOutput
      newForeignPtr_ ptrOutput >>= \foreignptrOutput ->
        return $ VS.unsafeFromForeignPtr0 foreignptrOutput ilen
      where wilen = itow32 ilen
            wnsec = itow32 nsec


calcInitCond :: ([Double],[Double]) -> [Double]
calcInitCond (num,denom) =
  let n = length num
   in head $ toLists $
       (ident (n-1) - fromBlocks [[-(1><(n-1)) (tail denom)], [fromBlocks [[ident (n-2), (1><(n-2)) $ replicate (n-2) 0]]]])
        <\> ((((n-1)><1) (tail num)) - scale (head num) (((n-1)><1) (tail denom)))


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

foreign import ccall "filterFunctions.h iir_filter" c_iir_filter :: Ptr CDouble -> CUInt ->  Ptr CDouble -> Ptr CDouble -> CUInt -> Ptr CDouble -> IO()

foreign import ccall "filterFunctions.h iir_filter_init" c_iir_filter_init :: Ptr CDouble -> CUInt ->  Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CUInt -> Ptr CDouble -> IO()

foreign import ccall "filterFunctions.h iir_filter_core" c_iir_filter_core :: Ptr CDouble -> CUInt ->  Ptr CDouble -> Ptr CDouble -> CUInt -> Ptr CDouble -> Ptr CDouble -> IO()

foreign import ccall "filterFunctions.h fir_filter" c_fir_filter :: Ptr CDouble -> CUInt ->  Ptr CDouble -> CUInt -> Ptr CDouble -> IO()

foreign import ccall "filterFunctions.h fir_filtfilt" c_fir_filtfilt :: Ptr CDouble -> CUInt ->  Ptr CDouble -> CUInt -> Ptr CDouble -> IO()

foreign import ccall "filterFunctions.h fir_filter_init" c_fir_filter_init :: Ptr CDouble -> CUInt ->  Ptr CDouble -> Ptr CDouble -> CUInt -> Ptr CDouble -> IO()

foreign import ccall "filterFunctions.h fir_filtfilt_init" c_fir_filtfilt_init :: Ptr CDouble -> CUInt ->  Ptr CDouble -> Ptr CDouble -> CUInt -> Ptr CDouble -> IO()

foreign import ccall "filterFunctions.h filtfilt" c_filtfilt :: Ptr CDouble -> CUInt ->  Ptr CDouble -> Ptr CDouble -> CUInt -> Ptr CDouble -> IO()

foreign import ccall "filterFunctions.h filtfilt_init" c_filtfilt_init :: Ptr CDouble -> CUInt ->  Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CUInt -> Ptr CDouble -> IO()

foreign import ccall "filterFunctions.h sosfilter" c'sosfilter :: Ptr CDouble -> CUInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CUInt -> Ptr CDouble -> IO()

foreign import ccall "filterFunctions.h sosfiltfilt" c'sosfiltfilt :: Ptr CDouble -> CUInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CUInt -> Ptr CDouble -> IO()

foreign import ccall "filterFunctions.h sosfilter_init" c'sosfilter_init :: Ptr CDouble -> CUInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CUInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO()

foreign import ccall "filterFunctions.h sosfiltfilt_init" c'sosfiltfilt_init :: Ptr CDouble -> CUInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CUInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO()
