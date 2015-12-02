
{-# LANGUAGE BangPatterns #-}


module GlitchMon.DataConditioning
(part'DataConditioning
) where


import Control.Monad.State (StateT, runStateT, execStateT, get, put, liftIO)
import Data.List (foldl')
import HasKAL.WaveUtils.Data hiding (detector, mean)
import HasKAL.SignalProcessingUtils.LinearPrediction (lpefCoeffV, whiteningWaveData)
import HasKAL.SpectrumUtils.SpectrumUtils (gwpsdV, gwOnesidedPSDV)
import Numeric.LinearAlgebra as NL

import qualified GlitchMon.GlitchParam as GP


part'DataConditioning :: WaveData
                     -> StateT GP.GlitchParam IO WaveData
part'DataConditioning wave = do
  param <- get
  let whtcoeff = GP.whtCoeff param
  case (whtcoeff /= []) of
    False -> do (whtCoeffList, rfwave) <- section'Whitening
                put $ GP.updateGlitchParam'whtCoeff param whtCoeffList
                put $ GP.updateGlitchParam'refpsd param
                  (gwpsdV (gwdata rfwave) (GP.refpsdlen param) (GP.samplingFrequency param))
                return $ applyWhitening whtCoeffList wave
    True  -> return $ applyWhitening whtcoeff wave


section'LineRemoval = id


section'Whitening :: StateT GP.GlitchParam IO ([([Double],  Double)],  WaveData)
section'Whitening = do
  param <- get
  liftIO $ calcWhiteningCoeff param


calcWhiteningCoeff :: GP.GlitchParam
              -> IO ([([Double], Double)], WaveData)
calcWhiteningCoeff param = do
  let refwave = GP.refwave param
  calcWhiteningCoeffCore param ([], refwave) >>=
    \(whtCoeffList, whtref) ->
    case checkingWhitening whtref of
      False -> calcWhiteningCoeffCore param (whtCoeffList, whtref)
      True -> return (whtCoeffList, whtref)


calcWhiteningCoeffCore :: GP.GlitchParam
              -> ([([Double], Double)], WaveData)
              -> IO ([([Double], Double)], WaveData)
calcWhiteningCoeffCore param (whtCoeffList, train) =
  let nC = GP.whtfiltordr param
      nfft = GP.refpsdlen param
      fs = GP.samplingFrequency param
      refpsd = gwpsdV (gwdata train) nfft fs
      whtCoeff' = lpefCoeffV nC refpsd
   in return ( whtCoeff':whtCoeffList
      , dropWaveData (2*nC) $ whiteningWaveData whtCoeff' train
      )


checkingWhitening wave = std (NL.toList (gwdata wave))  < 2.0


applyWhitening :: [([Double],  Double)]
               -> WaveData
               -> WaveData
applyWhitening [] wave = wave
applyWhitening (x:xs) wave =
  applyWhitening xs $ dropWaveData ((*2).length.fst $ x) $ whiteningWaveData x wave


{--------------------
- Helper Functions  -
--------------------}


mean :: Floating a => [a] -> a
mean x = fst $ foldl' (\(!m,  !n) x -> (m+(x-m)/(n+1), n+1)) (0, 0) x


var :: (Fractional a, Floating a) => [a] -> a
var xs = Prelude.sum (map (\x -> (x - mu)^(2::Int)) xs)  / (n - 1)
    where mu = mean xs
          n = fromIntegral $ length xs

std :: (RealFloat a) => [a] -> a
std x = sqrt $ var x


