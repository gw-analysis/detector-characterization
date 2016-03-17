
{-# LANGUAGE BangPatterns #-}


module GlitchMon.DataConditioning
(part'DataConditioning
) where


import Control.Monad.State (StateT, runStateT, execStateT, get, put, liftIO)
import Data.List (foldl')
import HasKAL.SignalProcessingUtils.LinearPrediction (lpefCoeffV, whiteningWaveData, whiteningWaveData')
import HasKAL.SignalProcessingUtils.Interpolation (interpV)
import HasKAL.SignalProcessingUtils.InterpolationType
import HasKAL.SpectrumUtils.SpectrumUtils (gwpsdV, gwOnesidedPSDV, gwOnesidedPSDWaveData, gwspectrogramWaveData)
import HasKAL.WaveUtils.Data hiding (detector, mean)
import Numeric.LinearAlgebra as NL

import qualified GlitchMon.GlitchParam as GP

import qualified HasKAL.PlotUtils.HROOT.PlotGraph3D as H3
import qualified HasKAL.PlotUtils.HROOT.PlotGraph as H


data WhnMethod = TimeDomain | FrequencyDomain


part'DataConditioning :: WaveData
                     -> StateT GP.GlitchParam IO WaveData
part'DataConditioning wave = do
  liftIO $ print "start data conditioning"
  param <- get
  let whtcoeff = GP.whtCoeff param
  case (whtcoeff /= []) of
    False -> do out <- section'Whitening TimeDomain wave
                liftIO $ H3.spectrogramM H3.LogY
                                         H3.COLZ
                                         "mag"
                                         "whitened data"
                                         "production/gw150914_whitened_spectrogram.png"
                                         ((0, 0), (20, 400))
                                         $ gwspectrogramWaveData 0.19 0.2 out
                liftIO $ H.plot H.Linear
                                H.Line
                                1
                                H.RED
                                ("time","amplitude")
                                0.05
                                "whitened data"
                                "production/gw150914_whitened_timeseries.png"
                                ((16.05,16.2),(0,0))
                                $ zip [0,1/4096..] (NL.toList $ gwdata out)
                liftIO $ H.plotV H.LogXY
                                 H.Line
                                 1
                                 H.RED
                                 ("frequency [Hz]","ASD [Hz^-1/2]")
                                 0.05
                                 "whitened data spectrum"
                                 "production/gw150914_whitened_spectrum.png"
                                 ((0,0),(0,0))
                                 $ gwOnesidedPSDWaveData 0.2 out
                return out
    True  -> section'Whitening TimeDomain wave


section'LineRemoval = id


section'Whitening :: WhnMethod -> WaveData -> StateT GP.GlitchParam IO WaveData
section'Whitening opt wave = case opt of
  TimeDomain 
    -> do param <- get
          (whtCoeffList, rfwave) <- liftIO $ calcWhiteningCoeff param
          put $ GP.updateGlitchParam'whtCoeff param whtCoeffList
          put $ GP.updateGlitchParam'refpsd param 
            (gwOnesidedPSDV (gwdata rfwave) (GP.refpsdlen param) (GP.samplingFrequency param))
          return $ applyWhitening TimeDomain whtCoeffList wave
  FrequencyDomain
    -> do param <- get
          (whtCoeffList, rfwave) <- liftIO $ calcWhiteningCoeff param
          put $ GP.updateGlitchParam'whtCoeff param whtCoeffList
          put $ GP.updateGlitchParam'refpsd param
            (gwOnesidedPSDV (gwdata rfwave) (GP.refpsdlen param) (GP.samplingFrequency param))
          return $ applyWhitening FrequencyDomain whtCoeffList wave      



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


applyWhitening :: WhnMethod
               -> [([Double],  Double)]
               -> WaveData
               -> WaveData
applyWhitening opt [] wave = wave
applyWhitening opt (x:xs) wave = case opt of
  TimeDomain -> 
    applyWhitening TimeDomain xs $ dropWaveData ((*2).length.fst $ x) $ whiteningWaveData x wave
  FrequencyDomain -> 
    applyWhitening FrequencyDomain xs $ whiteningWaveData' x wave


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


