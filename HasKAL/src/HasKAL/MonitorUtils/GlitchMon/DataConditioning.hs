
{-# LANGUAGE BangPatterns #-}


module HasKAL.MonitorUtils.GlitchMon.DataConditioning
(part'DataConditioning
, part'DataConditioning'
) where


import Control.Monad.State (StateT, runStateT, execStateT, get, put, liftIO)
import Data.List (foldl')
import Data.Maybe (fromJust)
import qualified Data.Vector.Storable as V
import HasKAL.SignalProcessingUtils.Cascade
import HasKAL.SignalProcessingUtils.Chebyshev(chebyshev1)
import HasKAL.SignalProcessingUtils.ButterWorth(butter)
import HasKAL.SignalProcessingUtils.FilterX(filtfilt0)
import HasKAL.SignalProcessingUtils.FilterType
import HasKAL.SignalProcessingUtils.ButterWorth
import HasKAL.SignalProcessingUtils.LinearPrediction (lpefCoeffV, whiteningWaveData, whiteningWaveData')
import HasKAL.SignalProcessingUtils.Interpolation (interpV)
import HasKAL.SignalProcessingUtils.InterpolationType
import HasKAL.SpectrumUtils.SpectrumUtils (gwpsdV, gwOnesidedPSDV, gwOnesidedPSDWaveData, gwspectrogramWaveData)
import HasKAL.WaveUtils.Data hiding (detector, mean)
import Numeric.LinearAlgebra as NL

import qualified HasKAL.MonitorUtils.GlitchMon.GlitchParam as GP

import qualified HasKAL.PlotUtils.HROOT.PlotGraph3D as H3
import qualified HasKAL.PlotUtils.HROOT.PlotGraph as H
import System.IO (hFlush, stdout)
import Control.DeepSeq (deepseq, NFData)


--instance NFData WaveData
deg = 2

part'DataConditioning :: WaveData
                     -> StateT GP.GlitchParam IO WaveData
part'DataConditioning wave = do
  liftIO $ print "start data conditioning" >> hFlush stdout
  param <- get
  {- High pass filtering at cutoff frequency in param-}
  let highpassed = filtfilt0 hpf (gwdata wave)
      hpf = chebyshev1 deg 0.1 fs newfs2 High
      --hpf = butter 4 fs newfs2 High
      newfs2 = 2*fs*tan (2*pi*newfs/fs/2)/(2*pi)
      newfs = GP.cutoffFreq param
      fs = GP.samplingFrequency param
  liftIO $ print "-- high-pass filtering" >> hFlush stdout
--  liftIO $ print $ "["++show (highpassed V.!0)++", "++show (highpassed V.!1)
--    ++", "++show (highpassed V.!2)++", "++show (highpassed V.!3)++"...]"
  highpassed `deepseq` return ()
  let highpassedw' = fromJust $ updateWaveDatagwdata wave highpassed
      highpassedw = dropWaveData (4*2*deg)
                      $ takeWaveData (lengthWaveData highpassedw' - 4*2*deg)
                      $ highpassedw'

  case GP.DC `elem` GP.debugmode param of
    True -> do let dir = GP.debugDir param
               liftIO $ H.plot H.Linear
                 H.Line
                 1
                 H.RED
                 ("time","amplitude")
                 0.05
                 "high-pass filtered data"
                 (dir++"/highpassed_timeseries.png")
                 ((0,0),(0,0))
                 $ zip [0,1/fs..] (NL.toList $ gwdata highpassedw)

               liftIO $ H.plotV H.LogXY
                  H.Line
                  1
                  H.RED
                  ("frequency [Hz]","ASD [Hz^-1/2]")
                  0.05
                  "spectrum of hig-hpass filtered timeseries"
                  (dir++"/highpassed_spectrum.png")
                  ((0,0),(0,0))
                  $ gwOnesidedPSDWaveData 1 highpassedw

    False -> liftIO $ Prelude.return ()

  let dir = GP.debugDir param
  let whtcoeff = GP.whtCoeff param
      wmethod   = GP.whnMethod param
  case (whtcoeff /= []) of
   -- | [TODO] at present the condition is not used because the iKAGRA data is not stationary enough.
    _     -> case GP.WH `elem` GP.debugmode param of
               True -> do
                 liftIO $ print "-- whitening... "
                 out <- section'Whitening wmethod highpassedw
                 liftIO $ H3.spectrogramM H3.LogY
                                          H3.COLZ
                                          "mag"
                                          "whitened data"
                                          "production/whitened_spectrogram.png"
                                          ((0, 0), (0, 0))
                                          $ gwspectrogramWaveData 0.1 0.2 out
                 liftIO $ H.plot H.Linear
                                 H.Line
                                 1
                                 H.RED
                                 ("time","amplitude")
                                 0.05
                                 "data"
                                 "production/whitened_timeseries.png"
                                 ((0,0),(0,0))
                                 $ zip [0,1..] (NL.toList $ gwdata out)
                 liftIO $ H.plotV H.LogXY
                                 H.Line
                                 1
                                 H.RED
                                 ("frequency [Hz]","ASD [Hz^-1/2]")
                                 0.05
                                 "whitened spectrum"
                                 (dir++"/whitened_spectrum.png")
                                 ((0,0),(0,0))
                                 $ gwOnesidedPSDWaveData 0.2 out
                 return out
               _ -> do
                       liftIO $ print "-- whitening... "
                       section'Whitening wmethod highpassedw
--                       out `deepseq` return out
--    True  -> section'Whitening wmethod wave



part'DataConditioning' :: WaveData
                       -> StateT GP.GlitchParam IO WaveData
part'DataConditioning' wave = do
  liftIO $ print "start data conditioning" >> hFlush stdout
  param <- get
  {- High pass filtering at cutoff frequency in param-}
  let highpassed = filtfilt0 hpf (gwdata wave)
      hpf = chebyshev1 deg 0.1 fs newfs2 High
      --hpf = butter 4 fs newfs2 High
      newfs2 = 2*fs*tan (2*pi*newfs/fs/2)/(2*pi)
      newfs = GP.cutoffFreq param
      fs = GP.samplingFrequency param
  liftIO $ print "-- high-pass filtering" >> hFlush stdout
--  liftIO $ print $ "["++show (highpassed V.!0)++", "++show (highpassed V.!1)
--    ++", "++show (highpassed V.!2)++", "++show (highpassed V.!3)++"...]"
  highpassed `deepseq` return ()
  let highpassedw' = fromJust $ updateWaveDatagwdata wave highpassed
      highpassedw = dropWaveData (4*2*deg)
                      $ takeWaveData (lengthWaveData highpassedw' - 4*2*deg)
                      $ highpassedw'

  case GP.DC `elem` GP.debugmode param of
    True -> do let dir = GP.debugDir param
               liftIO $ H.plot H.Linear
                 H.Line
                 1
                 H.RED
                 ("time","amplitude")
                 0.05
                 "high-pass filtered data"
                 (dir++"/highpassed_timeseries.png")
                 ((0,0),(0,0))
                 $ zip [0,1/fs..] (NL.toList $ gwdata highpassedw)

               liftIO $ H.plotV H.LogXY
                  H.Line
                  1
                  H.RED
                  ("frequency [Hz]","ASD [Hz^-1/2]")
                  0.05
                  "spectrum of hig-hpass filtered timeseries"
                  (dir++"/highpassed_spectrum.png")
                  ((0,0),(0,0))
                  $ gwOnesidedPSDWaveData 1 highpassedw

    False -> liftIO $ Prelude.return ()

  let dir = GP.debugDir param
  let whtcoeff = GP.whtCoeff param
      wmethod   = GP.whnMethod param
  case (whtcoeff /= []) of
   -- | [TODO] at present the condition is not used because the iKAGRA data is not stationary enough.
    _     -> case GP.WH `elem` GP.debugmode param of
               True -> do
                 liftIO $ print "-- whitening... "
                 out <- section'Whitening' wmethod highpassedw
                 liftIO $ H3.spectrogramM H3.LogY
                                          H3.COLZ
                                          "mag"
                                          "whitened data"
                                          "production/whitened_spectrogram.png"
                                          ((0, 0), (0, 0))
                                          $ gwspectrogramWaveData 0.1 0.2 out
                 liftIO $ H.plot H.Linear
                                 H.Line
                                 1
                                 H.RED
                                 ("time","amplitude")
                                 0.05
                                 "data"
                                 "production/whitened_timeseries.png"
                                 ((0,0),(0,0))
                                 $ zip [0,1..] (NL.toList $ gwdata out)
                 liftIO $ H.plotV H.LogXY
                                 H.Line
                                 1
                                 H.RED
                                 ("frequency [Hz]","ASD [Hz^-1/2]")
                                 0.05
                                 "whitened spectrum"
                                 (dir++"/whitened_spectrum.png")
                                 ((0,0),(0,0))
                                 $ gwOnesidedPSDWaveData 0.2 out
                 return out
               _ -> do
                       liftIO $ print "-- whitening... "
                       section'Whitening' wmethod highpassedw
--                       out `deepseq` return out
--    True  -> section'Whitening wmethod wave

section'LineRemoval = id


section'Whitening :: GP.WhnMethod
                  -> WaveData
                  -> StateT GP.GlitchParam IO WaveData
section'Whitening opt wave = case opt of
  GP.TimeDomain
    -> do param <- get
          (whtCoeffList, rfwave) <- liftIO $ calcWhiteningCoeff param
          put $ GP.updateGlitchParam'whtCoeff param whtCoeffList
          let whned = applyWhitening GP.TimeDomain whtCoeffList wave
              nfft = floor $ GP.nfrequency param * GP.samplingFrequency param
          put $ GP.updateGlitchParam'refpsd param
            (gwOnesidedPSDV (gwdata whned) nfft (GP.samplingFrequency param))
          liftIO $ print "---- Time-domain whitening..." >> hFlush stdout
          return whned
  GP.FrequencyDomain
    -> do liftIO $ print "---- Frequency-domain whitening..." >> hFlush stdout
          return wave
--          param <- get
--          (whtCoeffList, rfwave) <- liftIO $ calcWhiteningCoeff param
--          put $ GP.updateGlitchParam'whtCoeff param whtCoeffList
--          let whned = applyWhitening GP.FrequencyDomain whtCoeffList wave
--              nfft = floor $ GP.nfrequency param * GP.samplingFrequency param
--          put $ GP.updateGlitchParam'refpsd param
--            (gwOnesidedPSDV (gwdata whned) nfft (GP.samplingFrequency param))
--          return whned



section'Whitening' :: GP.WhnMethod
                   -> WaveData
                   -> StateT GP.GlitchParam IO WaveData
section'Whitening' opt wave = case opt of
  GP.TimeDomain
    -> do param <- get
          (whtCoeffList, rfwave) <- liftIO $ calcWhiteningCoeff param
--          liftIO $ print whtCoeffList  >> hFlush stdout
          put $ GP.updateGlitchParam'whtCoeff param whtCoeffList
          let whned = applyWhitening GP.TimeDomain whtCoeffList wave
              nfft = floor $ GP.nfrequency param * GP.samplingFrequency param
          put $ GP.updateGlitchParam'refpsd param
            (gwOnesidedPSDV (gwdata whned) nfft (GP.samplingFrequency param))
          liftIO $ print "---- Time-domain whitening..." >> hFlush stdout
          return whned
  GP.FrequencyDomain
    -> do liftIO $ print "---- Frequency-domain whitening..." >> hFlush stdout
          param <- get
          (whtCoeffList, rfwave) <- liftIO $ calcWhiteningCoeff param
          put $ GP.updateGlitchParam'whtCoeff param whtCoeffList
          let whned = applyWhitening GP.FrequencyDomain whtCoeffList wave
              nfft = floor $ GP.nfrequency param * GP.samplingFrequency param
--          liftIO $ print $ take 5 $ V.toList (gwdata whned)
          put $ GP.updateGlitchParam'refpsd param
            (gwOnesidedPSDV (gwdata whned) nfft (GP.samplingFrequency param))
          return whned


calcWhiteningCoeff :: GP.GlitchParam
                   -> IO ([([Double], Double)], WaveData)
calcWhiteningCoeff param = do
  let refwave = GP.refwave param
  calcWhiteningCoeffCore param ([], refwave)
--  >>=
--    \(whtCoeffList, whtref) ->
--    case checkingWhitening whtref of
--      False -> calcWhiteningCoeffCore param (whtCoeffList, whtref)
--      True -> return (whtCoeffList, whtref)


calcWhiteningCoeffCore :: GP.GlitchParam
                       -> ([([Double], Double)], WaveData)
                       -> IO ([([Double], Double)], WaveData)
calcWhiteningCoeffCore param (whtCoeffList, train) = do
  let nC = floor $ 2 * fs / GP.whnFrequencyResolution param
      fs = samplingFrequency train
      nfft = floor $ fs * GP.traindatlen param
      refpsd = gwpsdV (gwdata train) nfft fs
      whtCoeff' = lpefCoeffV nC refpsd
--  return ( whtCoeff':whtCoeffList
  return ( [whtCoeff']
      , whiteningWaveData whtCoeff' train
--      , dropWaveData (2*nC) $ whiteningWaveData whtCoeff' train
      )


checkingWhitening wave = std (NL.toList (gwdata wave))  < 2.0


applyWhitening :: GP.WhnMethod
               -> [([Double],  Double)]
               -> WaveData
               -> WaveData
applyWhitening opt [] wave = wave
applyWhitening opt (x:xs) wave = case opt of
  GP.TimeDomain ->
--    applyWhitening TimeDomain xs $ dropWaveData ((*2).length.fst $ x) $ whiteningWaveData x wave
    applyWhitening GP.TimeDomain xs $ whiteningWaveData x wave
  GP.FrequencyDomain ->
    applyWhitening GP.FrequencyDomain xs $ whiteningWaveData' x wave


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
