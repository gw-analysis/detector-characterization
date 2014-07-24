{-
- - test code to check iirFilter and butter in SignalProcessingUtils
- - to compile the code,  run
- - ghc -o testFilter testFilter.hs
- HasKAL/SignalProcessingUtils/filterFunctions.c
- - -}
--
-- import HasKAL.SignalProcessingUtils.Filter
-- import HasKAL.SignalProcessingUtils.ButterWorth
-- import HasKAL.SignalProcessingUtils.FilterType
-- import HasKAL.SpectrumUtils.GwPsdMethod
-- import HasKAL.SpectrumUtils.SpectrumUtils
-- import System.Random
--
-- import HasKAL.PlotUtils.PlotOption.PlotOptionHROOT
-- import HasKAL.PlotUtils.PlotUtilsHROOT
--
-- main :: IO ()
-- main = do
--   let x = take 1000 $ randomRs (-1, 1) $ mkStdGen 1 :: [Double]
--       (numCoeffLow,denomCoeffLow) = butter 2 100 20 Low
--       (numCoeffHigh,denomCoeffHigh) = butter 2 100 20 High
--       y = iirFilter x (length x) numCoeffLow denomCoeffLow (length numCoeffLow)
--       z = iirFilter x (length x) numCoeffHigh denomCoeffHigh (length numCoeffHigh)
--
--   let out1 = gwpsd x 100 100
--       out2 = gwpsd y 100 100
--       out3 = gwpsd z 100 100
--
--   logLogPlot (map fst out1) (map snd out1) "frequency[Hz]" "asd[1/rHz]" Line "beforeFiltered.pdf"
--   logLogPlot (map fst out2) (map snd out2) "frequency[Hz]" "asd[1/rHz]" Line "afterLowPassFiltered.pdf"
--   logLogPlot (map fst out3) (map snd out3) "frequency[Hz]" "asd[1/rHz]" Line "afterHighPassFiltered.pdf"



{-# LANGUAGE ForeignFunctionInterface #-}

module HasKAL.SignalProcessingUtils.Filter
  (
  iirFilter,
  firFilter,
  filtfilt
  ) where

import Foreign.C.Types
-- import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Array
import System.IO.Unsafe



iirFilter :: [Double] -> Int -> [Double] -> [Double] -> Int -> [Double]
iirFilter input ilen numCoeff denomCoeff flen = do
  let input' = d2cd input
      numCoeff' = d2cd numCoeff
      denomCoeff' = d2cd denomCoeff
  cd2d $ iirFilterCore input' ilen numCoeff' denomCoeff' flen

firFilter :: [Double] -> Int -> [Double] -> Int -> [Double]
firFilter input ilen firCoeff flen = do
  let input' = d2cd input
      firCoeff' = d2cd firCoeff
  cd2d $ firFilterCore input' ilen firCoeff' flen


filtfilt :: [Double] -> [Double] -> [Double] -> [Double]
filtfilt input numCoeff denomCoeff = do
--  let inputCD = d2cd input
  let numCoeffCD = d2cd numCoeff
      denomCoeffCD = d2cd denomCoeff
      ilen = length input
      flen = length numCoeff
      lrefl = 3 * (flen - 1)
      si'' = tail.reverse.cumsum.reverse $ zipWith (-) numCoeff $ map (*((sum numCoeff)/(sum denomCoeff))) denomCoeff
      si' = 0:si''
      si = d2cd si'

      input'' = (map (2*(head input)-) $ reverse $ foldl (\acc m -> (input !! m):acc) [] [lrefl,lrefl-1..2])
        ++ input ++ (map (2*(last input)-) $ reverse $ foldl (\acc m -> (input !! m):acc) [] [ilen-1, ilen-2..ilen-lrefl])
      input' = d2cd input''
      forwardFiltered = iirFilterCoreInit input' (length input') numCoeffCD denomCoeffCD flen (map (*(head input')) si)
      reverseFiltered = reverse $ iirFilterCoreInit (reverse forwardFiltered)
        (length forwardFiltered) numCoeffCD denomCoeffCD flen (map (*(last forwardFiltered)) si)
  reverse $ foldl (\acc m -> (input !! m):acc) [] [lrefl+1..lrefl+ilen]


-------------  Internal Functions  -----------------------------
cumsum :: [Double] -> [Double]
cumsum xs = scanl1 (+) xs

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


iirFilterCoreInit :: [CDouble] -> Int -> [CDouble] -> [CDouble] -> Int -> [CDouble] -> [CDouble]
iirFilterCoreInit input ilen numCoeff denomCoeff flen initCoeff
  = unsafePerformIO $ withArray input $ \ptrInput ->
   withArray numCoeff $ \ptrNumCoeff ->
   withArray denomCoeff $ \ptrDenomCoeff ->
   withArray initCoeff $ \ptrInitCoeff ->
   allocaArray ilen $ \ptrOutput ->
   do c_iir_filter_core ptrInput wilen ptrNumCoeff ptrDenomCoeff wflen ptrInitCoeff ptrOutput
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


itow32 :: Int -> CUInt
itow32 = fromIntegral

d2cd :: [Double] -> [CDouble]
d2cd = map realToFrac

cd2d :: [CDouble] -> [Double]
cd2d = map realToFrac


foreign import ccall "filterFunctions.h iir_filter" c_iir_filter :: Ptr CDouble -> CUInt ->  Ptr CDouble -> Ptr CDouble -> CUInt -> Ptr CDouble -> IO()

foreign import ccall "filterFunctions.h iir_filter_core" c_iir_filter_core :: Ptr CDouble -> CUInt ->  Ptr CDouble -> Ptr CDouble -> CUInt -> Ptr CDouble -> Ptr CDouble -> IO()

foreign import ccall "filterFunctions.h fir_filter" c_fir_filter :: Ptr CDouble -> CUInt ->  Ptr CDouble -> CUInt -> Ptr CDouble -> IO()


