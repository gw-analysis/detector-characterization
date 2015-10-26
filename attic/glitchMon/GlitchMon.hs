

module GlitchMon
( runGlitchMon
--,
)
where


import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad ((>>=))
import Control.Monad.State (StateT, runStateT, execStateT, get, put, liftIO)
import Data.Conduit (yield,  await)
import qualified Data.Conduit.List as CL
import Data.List (nub)
import qualified Data.Set as Set
import Filesystem.Path (extension)
import Filesystem.Path.CurrentOS (decodeString,  encodeString)
import HasKAL.FrameUtils.FrameUtils (getGPSTime)
import HasKAL.FrameUtils.Function (readFrameWaveData)
import HasKAL.MathUtils.FFTW (dct2d, idct2d)
import HasKAL.SpectrumUtils.Function (updateMatrixElement, updateSpectrogramSpec)
import HasKAL.SpectrumUtils.Signature (Spectrum, Spectrogram)
import HasKAL.SpectrumUtils.SpectrumUtils (gwpsdV, gwOnesidedPSDV)
import HasKAL.SignalProcessingUtils.LinearPrediction (lpefCoeffV, whiteningWaveData)
import HasKAL.TimeUtils.Function (formatGPS, deformatGPS)
import HasKAL.WaveUtils.Data hiding (detector)
import HasKAL.WaveUtils.Signature
import Numeric.LinearAlgebra as NL
import System.FSNotify (withManager, watchDirChan)

import qualified GlitchParam as GP
import PipelineFunction
import Data (TrigParam (..))
import RegisterGlitchEvent (registGlitchEvent2DB)




runGlitchMon watchdir chname s = runStateT go s
  where go = do param <- get
                runResourceT $ source watchdir $$ sink param chname


source watchdir = do
  maybefname <- withManager $ \manager ->
    watchDirChan manager watchdir (const True)
     $ \event -> case event of
      Removed _ _ -> print "file removed"
      _ -> case extension (decodeString $ eventPath event) of
             Just "filepart" -> do print "file downloading"
                                   return Nothing
             Just "gwf" -> do
               let gwfname = eventPath event
               return gwfname
             Nothing -> do print "file extension should be .filepart or .gwf"
                           return Nothing
  case maybefname of
    Nothing -> source
    Just fname -> yield fname >> source


sink param chname = do
  c <- await
  case c of
    Nothing -> sink
    Just fname -> do
      maybegps <- getGPSTime fname
      case maybegps of
        Nothing -> sink
        Just (s, n, dt') -> do
          let gps = deformatGPS (s, n)
              dt = floor dt'
          maybewave <- readFrameWaveData "General" gps dt chname fname
          case maybewave of
            Nothing -> sink
            Just wave -> glitchMon param wave


glitchMon :: GP.GlitchParam
             -> WaveData
             -> StateT GP.GlitchParam IO ()
glitchMon param wave =
  runStateT part'DataConditioning w param >>= \(a, s) ->
    runStateT part'EventTriggerGeneration a s


part'DataConditioning :: WaveData
                     -> StateT GP.GlitchParam IO WaveData
part'DataConditioning wave = do
  param <- get
  let whtcoeff = GP.whtCoeff param
  case (whtcoeff /= []) of
    False -> do (whtCoeffList, refwave) <- section'Whitening wave
                put $ GP.updateParam param "whnCoeff" whtCoeffList
                put $ GP.updateParam param "refpsd" (gwpsdV (gwdata refwave))
                return $ applyWhitening whtCoeffList wave
    True  -> return $ applyWhitening whtcoeff wave


part'EventTriggerGeneration :: WaveData
                           -> StateT GP.GlitchParam IO ()
part'EventTriggerGeneration wave = do
  param <- get
  runStateT section'TimeFrequencyExpression wave param >>= \(a, s) ->
    runStateT section'Clustering a s >>= \(a', s') ->
      runStateT section'ParameterEstimation a' s' >>= \(a'', s'') ->
        runStateT section'RegisterEventtoDB a'' s''


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


checkingWhitening wave = std (NL.toList wave)  < 2.0


applyWhitening :: [([Double],  Double)]
               -> WaveData
               -> WaveData
applyWhitening [] wave = wave
applyWhitening (x:xs) wave =
  applyWhitening xs $ dropWaveData ((*2).length.fst $ x) $ whiteningWaveData x wave



section'TimeFrequencyExpression :: WaveData
                                -> StateT GP.GlitchParam IO Spectrogram
section'TimeFrequencyExpression whnWaveData = do
  param <- get
  let refpsd = GP.refpsd param
      fs = GP.samplingFrequency param
      nfreq2 = GP.nfrequency param`div`2
      nfreq = GP.nfrequency param
      ntime = GP.ntimeSlide param
      snrMatF = scale (fs/fromIntegral nfreq) $ fromList [0.0, 1.0..fromIntegral nfreq2]
      snrMatT = scale (fromIntegral nfreq/fs) $ fromList [0.0, 1.0..fromIntegral ntime -1]
      snrMatT' = deformatGPS (startGPSTime whnWaveData) + snrMatT
      snrMatP = (nfreq2><ntime) $ concatMap (\i -> map ((!! i) . (\i->toList $ zipVectorWith (/)
        (
        snd $ gwOnesidedPSDV (subVector (nfreq*i) nfreq (gwdata whnWaveData)) nfreq fs)
        (snd refpsd)
        )) [0..ntime-1]) [0..nfreq2] :: Matrix Double
  return (snrMatT', snrMatF, snrMatP)


section'Clustering :: Spectrogram
                   -> StateT GP.GlitchParam IO Spectrogram
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


section'ParameterEstimation :: Spectrogram
                            -> StateT GP.GlitchParam IO TrigParam
section'ParameterEstimation m = do
  param <- get
  let fs = GP.samplingFrequency param
  let (trigT, trigF, trigM) = m
      indxBlack = maxIndex trigM
      tsnr = trigM @@> indxBlack
      gps = formatGPS $ trigT @> fst indxBlack
      gpss = fromIntegral $ fst gps :: Int
      gpsn = fromIntegral $ snd gps :: Int
      fc = trigF @> snd indxBlack
      tfs = truncate fs :: Int
  return TrigParam { detector = Just "XE"
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


section'RegisterEventtoDB =
  registGlitchEvent2DB


mean :: Floating a => [a] -> a
mean x = fst $ foldl' (\(!m,  !n) x -> (m+(x-m)/(n+1), n+1)) (0, 0) x


var :: (Fractional a) => [a] -> a
var xs = Prelude.sum (map (\x -> (x - mu)^(2::Int)) xs)  / (n - 1)
    where mu = mean xs
    n = fromIntegral $ length $ xs

std :: (RealFloat a) => [a] -> a
std x = sqrt $ var x

