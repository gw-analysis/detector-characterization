




module GlitchMon
--( runGlitchMon
--,
--)
where


import Control.Monad ((>>=))
import Control.Monad.State (StateT, runStateT, execStateT, get, put, liftIO)
import qualified Data.Set as Set
import Data.List (nub)
import HasKAL.MathUtils.FFTW (dct2d, idct2d)
import HasKAL.SpectrumUtils.Function (updateMatrixElement, updateSpectrogramSpec)
import HasKAL.SpectrumUtils.Signature
import HasKAL.SpectrumUtils.SpectrumUtils(gwpsdV, gwOnesidedPSDV)
import HasKAL.SignalProcessingUtils.LinearPrediction(lpefCoeffV, whiteningWaveData)
import HasKAL.TimeUtils.Function (formatGPS)
import HasKAL.WaveUtils.Data hiding (detector)
import HasKAL.WaveUtils.Signature
import Numeric.LinearAlgebra as NL
import qualified GlitchParam as GP
import PipelineFunction
import Data (TrigParam (..))
import RegisterGlitchEvent (registGlitchEventCandidate2DB)


-- runGlitchMon :: WaveData
--              -> GP.GlitchParam
-- runGlitchMon w = flip execStateT param $ \w ->
--   partDataConditioning w


partDataConditioning :: WaveData
                     -> StateT GP.GlitchParam IO WaveData
partDataConditioning wave = do
  param <- get
  let whtcoeff = GP.whtCoeff param
  case (whtcoeff /= []) of
    False -> do (whtCoeffList, refwave) <- section'Whitening wave
                put $ GP.updateParam param "whnCoeff" whtCoeffList
                put $ GP.updateParam param "refpsd" (gwpsdV (gwdata refwave))
                return $ applyWhitening whtCoeffList wave
    True  -> return $ applyWhitening whtcoeff wave


section'LineRemoval = id

section'Whitening :: WaveData -> StateT GP.GlitchParam IO ([([Double],  Double)],  WaveData)
section'Whitening wave = do
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
      nfft = GP.chunklen param
      fs = GP.samplingFrequency param
      refpsd = gwpsdV (gwdata train) nfft fs
      whtCoeff' = lpefCoeffV nC refpsd
   in return ( whtCoeff':whtCoeffList
      , dropWaveData (2*nC) $ whiteningWaveData whtCoeff' train
      )


checkingWhitening = undefined


applyWhitening :: [([Double],  Double)]
               -> WaveData
               -> WaveData
applyWhitening [] wave = wave
applyWhitening (x:xs) wave =
  applyWhitening xs $ dropWaveData ((*2).length.fst $ x) $ whiteningWaveData x wave


-- partEventTriggerGeneration :: GP.GlitchParam
--                            -> WaveData
--                            -> IO ()
-- partEventTriggerGeneration param dat =
--   return $ section'TimeFrequencyExpression . section'Clustering . section'ParameterEstimation . section'RegisterParametertoDB $ dat
--

section'TimeFrequencyExpression :: WaveData
                                -> StateT GP.GlitchParam IO (NL.Vector Double, NL.Vector Double, NL.Matrix Double)
section'TimeFrequencyExpression whnWaveData = do
  param <- get
  let refpsd = GP.refpsd param
      fs = GP.samplingFrequency param
      nfreq2 = GP.nfrequency param`div`2
      nfreq = GP.nfrequency param
      ntime = GP.ntimeSlide param
      snrMatF = scale (fs/fromIntegral nfreq) $ fromList [0.0, 1.0..fromIntegral nfreq2]
      snrMatT = scale (fromIntegral nfreq/fs) $ fromList [0.0, 1.0..fromIntegral ntime -1]
      snrMatP = (nfreq2><ntime) $ concatMap (\i -> map ((!! i) . (\i->toList $ zipVectorWith (/)
        (
        snd $ gwOnesidedPSDV (subVector (nfreq*i) nfreq (gwdata whnWaveData)) nfreq fs)
        (snd refpsd)
        )) [0..ntime-1]) [0..nfreq2] :: Matrix Double
  return (snrMatT, snrMatF, snrMatP)


section'Clustering :: (NL.Vector Double,  NL.Vector Double,  NL.Matrix Double)
                   -> StateT GP.GlitchParam IO (NL.Vector Double,  NL.Vector Double,  NL.Matrix Double)
section'Clustering (snrMatT, snrMatF, snrMatP') = do
  param <- get
  let dcted' = dct2d snrMatP'
      ncol = cols dcted'
      nrow = rows dcted'
      zeroElementc = [(x, y) | x<-[0..nrow-1], y<-[ncol-GP.resolvTime param..ncol-1]]
      zeroElementr = [(x, y) | y<-[0..ncol-1], x<-[nrow-GP.resolvFreq param..nrow-1]]
      zeroElement = zeroElementr ++ zeroElementc
      dcted = updateMatrixElement dcted' zeroElement $ take (length zeroElement) [0, 0..]
      snrMatP = idct2d dcted
      thresIndex = head $ NL.find (>=GP.cutoffFreq param) snrMatF
      snrMat = (snrMatT, subVector thresIndex (nrow-thresIndex) snrMatF, dropRows thresIndex snrMatP)
      (_, _, mg) = snrMat
      thrsed = NL.find (>=GP.clusterThres param) mg
      survivor = nub $ excludeOnePixelIsland thrsed
      excludedIndx = Set.toList $ Set.difference (Set.fromList thrsed) (Set.fromList survivor)
      newM = updateSpectrogramSpec snrMat
       $ updateMatrixElement mg excludedIndx (replicate (length excludedIndx) 0.0)
  return newM


section'ParameterEstimation whnWaveData m = do
  param <- get
  let fs = GP.samplingFrequency param
  let (trigT, trigF, trigM) = m
      indxBlack = maxIndex trigM
      tsnr = trigM @@> indxBlack
      gps' = formatGPS $ trigT @> fst indxBlack
      gps = (fst (startGPSTime whnWaveData)+fst gps', snd (startGPSTime whnWaveData)+snd gps')
      gpss = fromIntegral $ fst gps :: Int
      gpsn = fromIntegral $ snd gps :: Int
      fc = trigF @> snd indxBlack
      tfs = fromIntegral $ truncate fs :: Int
  return   TrigParam { detector = Just "XE"
                     , event_gpsstarts = Just gpss
                     , event_gpsstartn = Just gpsn
                     , event_gpsstops  = Nothing
                     , event_gpsstopn  = Nothing
                     , duration = Nothing
                     , energy = Nothing
                     , central_frequency = Just fc
                     , snr = Just tsnr
                     , significance = Nothing
                     , latitude = Nothing
                     , longitude = Nothing
                     , chname = Nothing
                     , sampling_rate = Just tfs
                     , segment_gpsstarts = Nothing
                     , segment_gpsstartn = Nothing
                     , segment_gpsstops = Nothing
                     , segment_gpsstopn = Nothing
                     , dq_flag = Nothing
                     , pipeline = Just "iKAGRA Burst pipeline"
                     }


section'RegisterParametertoDB =
  registGlitchEventCandidate2DB



