




module GlitchMon
( runGlitchMon
--,
) where


import Control.Monad ((>>=))
import Control.Monad.State (runState)
import qualified Data.Set as Set
import Data.List (nub)
import HasKAL.MathUtils.FFTW (dct2d, idctd)
import HasKAL.SpectrumUtils.Function (updateMatrixElement, updateSpectrogramSpec)
import HasKAL.SpectrumUtils.Signature
import HasKAL.SpectrumUtils.SpectrumUtils(gwpsdV, gwOnesidedPSDV)
import HasKAL.SignalProcessingUtils.LinearPrediction(lpefCoeffV, whiteningWaveData)
import HasKAL.WaveUtils.Data
import HasKAL.WaveUtils.Signature
import Numeric.LinearAlgebra as NL
import qualified GlitchParam as GP
import PipelineFunction

runGlitchMon = undefined


partDataConditioning param refwave wave =
  section'Whitening . section'LineRemoval


section'LineRemoval = id


section'Whitening = id


calcWhiteningCoeff :: GP.GlitchParam
              -> WaveData
              -> IO ([([Double], Double)], WaveData)
calcWhiteningCoeff param refwave = do
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
      nfft = GP.chunklen param
      fs = GP.samplingFrequency param
      refpsd = gwpsdV (gwdata train) nfft fs
      whtCoeff' = lpefCoeffV nC refpsd
   in return ( whtCoeff':whtCoeffList
      , dropWaveData (2*nC) $ whiteningWaveData whtCoeff' train
      )


checkingWhitening = undefined


-- partEventTriggerGeneration :: GP.GlitchParam
--                            -> WaveData
--                            -> IO ()
-- partEventTriggerGeneration param dat =
--   return $ section'TimeFrequencyExpression . section'Clustering . section'ParameterEstimation . section'RegisterParametertoDB $ dat
--

section'TimeFrequencyExpression param refpsd whnWaveData =
  let fs = GP.samplingFrequency param
      nfreq2 = GP.nfrequency param`div`2
      nfreq = GP.nfrequency param
      ntime = GP.ntimeSlide param
      snrMatF = scale (fs/fromIntegral nfreq) $ fromList [0.0, 1.0..fromIntegral nfreq2]
      snrMatT = scale (fromIntegral nfreq/fs) $ fromList [0.0, 1.0..fromIntegral ntime -1]
      snrMatP = (nfreq2><ntime) $ concatMap (\i -> map ((!! i) . (\i->toList $ zipVectorWith (/)
        (
        snd $ gwOnesidedPSDV (subVector (nfreq*i) nfreq (gwdata whnWaveData)) nfreq fs)
        refpsd
        )) [0..ntime-1]) [0..nfreq2] :: Matrix Double
   in (snrMatT, snrMatF, snrMatP)


section'Clustering param (snrMatT, snrMatF, snrMatP') =
  let dcted' = dct2d snrMatP'
      ncol = cols dcted'
      nrow = rows dcted'
      zeroElementc = [(x, y) | x<-[0..nrow-1], y<-[ncol-GP.resolvTime param..ncol-1]]
      zeroElementr = [(x, y) | y<-[0..ncol-1], x<-[nrow-GP.resolvFreq param..nrow-1]]
      zeroElement = zeroElementr ++ zeroElementc
      dcted = updateMatrixElement dcted' zeroElement $ take (length zeroElement) [0, 0..]
      snrMatP = idct2d dcted
      thresIndex = head $ Numeric.LinearAlgebra.find (>=GP.cutoffFreq param) snrMatF
      snrMat = (snrMatT, subVector thresIndex (nrow-thresIndex) snrMatF, dropRows thresIndex snrMatP)
      (_, _, mg) = snrMat
      thrsed = NL.find (>=GP.clusterThres param) mg
      survivor = nub $ excludeOnePixelIsland thrsed
      excludedIndx = Set.toList $ Set.difference (Set.fromList thrsed) (Set.fromList survivor)
      newM = updateSpectrogramSpec snrMat
        $ updateMatrixElement mg excludedIndx (replicate (length excludedIndx) 0.0)


section'ParameterEstimation  = undefined
section'RegisterParametertoDB = undefined



