




module GlitchMon
( runGlitchMon
--,
) where


import Control.Monad ((>>=))
import HasKAL.SpectrumUtils.Signature
import HasKAL.SpectrumUtils.SpectrumUtils(gwpsdV)
import HasKAL.SignalProcessingUtils.LinearPrediction(lpefCoeffV, whiteningWaveData)
import HasKAL.WaveUtils.Data
import HasKAL.WaveUtils.Signature
import qualified GlitchParam as GP


runGlitchMon = undefined


runGlitchMonCore :: GP.GlitchParam
                 -> WaveData
                 -> WaveData
                 -> IO()
runGlitchMonCore param refwave wave = do
  conddat <- partWhitening param ([], refwave) wave >>=
    \(whtCoeffList, whtref, whtwave) ->
    case checkingWhitening whtref of
      False ->
        do (_, _, c) <- partWhitening param (whtCoeffList, whtref) whtwave
           return c
      True -> return whtwave
  return ()


partWhitening :: GP.GlitchParam
              -> ([([Double], Double)], WaveData)
              -> WaveData
              -> IO ([([Double], Double)], WaveData, WaveData)
partWhitening param (whtCoeffList, train) input =
  let nC = GP.whtfiltordr param
      nfft = GP.chunklen param
      fs = GP.samplingFrequency param
      refpsd = gwpsdV (gwdata train) nfft fs
      whtCoeff' = lpefCoeffV nC refpsd
   in return ( whtCoeff':whtCoeffList
      , dropWaveData (2*nC) $ whiteningWaveData whtCoeff' train
      , dropWaveData (2*nC) $ whiteningWaveData whtCoeff' input)


checkingWhitening = undefined



