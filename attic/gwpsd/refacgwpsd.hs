



{-# OPTIONS_GHC -XBangPatterns #-}

-- module HasKAL.SpectrumUtils.SpectrumUtils
-- ( module HasKAL.SpectrumUtils.Function
-- , module HasKAL.SpectrumUtils.GwPsdMethod
-- , module HasKAL.SpectrumUtils.Signature
-- , gwpsdV
-- )
-- where

{- Signature -}
import HasKAL.SpectrumUtils.Signature
import HasKAL.SpectrumUtils.Function

{- For fft -}
import Numeric.GSL.Fourier
import Numeric.LinearAlgebra

{- psd method type -}
import HasKAL.SpectrumUtils.GwPsdMethod

{- for windowing -}
import HasKAL.SignalProcessingUtils.WindowType
import HasKAL.SignalProcessingUtils.WindowFunction

--import HasKAL.SpectrumUtils.AuxFunction(sort, median)
import Data.List (sort, foldl')






