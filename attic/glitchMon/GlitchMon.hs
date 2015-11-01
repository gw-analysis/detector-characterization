{-# LANGUAGE BangPatterns #-}


module GlitchMon.GlitchMon
( runGlitchMon
, eventDisplay
, eventDisplayF
)
where


import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar,takeMVar)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad ((>>=))
import Control.Monad.State (StateT, runStateT, execStateT, get, put, liftIO)
import Data.Conduit (bracketP, yield,  await, ($$), Source, Sink, Conduit)
import qualified Data.Conduit.List as CL
import Data.Int (Int32)
import Data.List (nub, foldl')
import qualified Data.Set as Set
import Data.Text ( pack )
import Filesystem.Path (extension)
import Filesystem.Path.CurrentOS (decodeString,  encodeString)
import HasKAL.DetectorUtils.Detector(Detector(..))
import HasKAL.FrameUtils.FrameUtils (getGPSTime)
import HasKAL.FrameUtils.Function (readFrameWaveData)
import HasKAL.MathUtils.FFTW (dct2d, idct2d)
import HasKAL.SpectrumUtils.Function (updateMatrixElement, updateSpectrogramSpec)
import HasKAL.SpectrumUtils.Signature (Spectrum, Spectrogram)
import HasKAL.SpectrumUtils.SpectrumUtils (gwpsdV, gwOnesidedPSDV)
import HasKAL.SignalProcessingUtils.LinearPrediction (lpefCoeffV, whiteningWaveData)
import HasKAL.TimeUtils.Function (formatGPS, deformatGPS)
import HasKAL.WaveUtils.Data hiding (detector, mean)
import HasKAL.WaveUtils.Signature
import Numeric.LinearAlgebra as NL
import System.FSNotify (startManager, stopManager, withManager, watchTree, Event(..), eventPath)

import qualified GlitchMon.GlitchParam as GP
import GlitchMon.PipelineFunction
import GlitchMon.Data (TrigParam (..))
import GlitchMon.RegisterGlitchEvent (registGlitchEvent2DB)




runGlitchMon watchdir param chname =
  source watchdir $$ sink param chname


source :: FilePath
       -> Source IO FilePath
source watchdir = do
  x <- liftIO $ withManager $ \manager -> do
    goC <- liftIO newEmptyMVar
    _ <- watchTree manager watchdir (const True)
     $ \event -> case event of
      Removed _ _ -> print "file removed"
      _ -> case extension (decodeString $ eventPath event) of
             Just filepart -> print "file downloading"
             Just gwf -> do
               let gwfname = eventPath event
               putMVar goC gwfname
             Nothing -> print "file extension should be .filepart or .gwf"
    takeMVar goC
  yield x >> source watchdir
  where filepart = pack "filepart"
        gwf = pack "gwf"


sink :: GP.GlitchParam
     -> String
     -> Sink String IO ()
sink param chname = do
  c <- await
  case c of
    Nothing -> return ()
    Just fname -> do
      maybegps <- liftIO $ getGPSTime fname
      case maybegps of
        Nothing -> return ()
        Just (s, n, dt') -> do
          let gps = floor $ deformatGPS (s, n)
              dt = floor dt'
          maybewave <- liftIO $ readFrameWaveData General gps dt chname fname
          case maybewave of
            Nothing -> return ()
            Just wave -> do s <- liftIO $ glitchMon param wave
                            sink s chname


glitchMon :: GP.GlitchParam
          -> WaveData
          -> IO GP.GlitchParam
glitchMon param w =
  runStateT (part'DataConditioning w) param >>= \(a, s) ->
    runStateT (part'EventTriggerGeneration a) s >>= \(a', s') ->
      runStateT (part'ParameterEstimation a') s' >>= \(a'', s'') ->
         case a'' of
           Just t -> part'RegisterEventtoDB t >> return s''
           Nothing -> return s''


eventDisplay :: GP.GlitchParam
             -> WaveData
             -> IO (Maybe TrigParam, GP.GlitchParam, Spectrogram)
eventDisplay param w =
  runStateT (part'DataConditioning w) param >>= \(a, s) ->
    runStateT (part'EventTriggerGeneration a) s >>= \(a', s') ->
       do (trigparam, param') <- runStateT (part'ParameterEstimation a') s'
          return (trigparam, param',a')


eventDisplayF :: GP.GlitchParam
              -> FilePath
              -> String
              -> IO (Maybe TrigParam, GP.GlitchParam, Spectrogram)
eventDisplayF param fname chname = do
  maybegps <- getGPSTime fname
  case maybegps of
    Nothing -> error "file broken"
    Just (s, n, dt') -> do
      let gps = floor $ deformatGPS (s, n)
          dt = floor dt'
      maybewave <- readFrameWaveData General gps dt chname fname
      case maybewave of
        Nothing -> error "file broken"
        Just w -> runStateT (part'DataConditioning w) param >>= \(a, s) ->
                    runStateT (part'EventTriggerGeneration a) s >>= \(a', s') ->
                       do (trigparam, param') <- runStateT (part'ParameterEstimation a') s'
                          return (trigparam, param',a')


part'DataConditioning :: WaveData
                     -> StateT GP.GlitchParam IO WaveData
part'DataConditioning wave = do
  param <- get
  let whtcoeff = GP.whtCoeff param
  case (whtcoeff /= []) of
    False -> do (whtCoeffList, rfwave) <- section'Whitening wave
                put $ GP.updateGlitchParam'whtCoeff param whtCoeffList
                put $ GP.updateGlitchParam'refpsd param
                  (gwpsdV (gwdata rfwave) (GP.chunklen param) (GP.samplingFrequency param))
                return $ applyWhitening whtCoeffList wave
    True  -> return $ applyWhitening whtcoeff wave


part'EventTriggerGeneration :: WaveData
                            -> StateT GP.GlitchParam IO Spectrogram
part'EventTriggerGeneration wave = do
  param <- get
  (a, s) <- liftIO $ runStateT (section'TimeFrequencyExpression wave) param
  section'Clustering a


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


checkingWhitening wave = std (NL.toList (gwdata wave))  < 2.0


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
      snrMatT' = mapVector (+deformatGPS (startGPSTime whnWaveData)) snrMatT
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


part'ParameterEstimation :: Spectrogram
                         -> StateT GP.GlitchParam IO (Maybe TrigParam)
part'ParameterEstimation m = do
  param <- get
  let fs = GP.samplingFrequency param
  let (trigT, trigF, trigM) = m
      mrow = NL.rows trigM
      mcol = NL.cols trigM
      zerom = (mrow >< mcol) (replicate (mrow*mcol) (0::Double))
  case (trigM == zerom) of
    True -> do
      let indxBlack = maxIndex trigM
          tsnr = trigM @@> indxBlack
          gps = formatGPS $ trigT @> fst indxBlack
          gpss = fromIntegral $ fst gps :: Int32
          gpsn = fromIntegral $ snd gps :: Int32
          fc = trigF @> snd indxBlack
          tfs = fromIntegral $ truncate fs :: Int32
      return $ Just TrigParam { detector = Just "XE"
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
    False -> return Nothing

part'RegisterEventtoDB =
  registGlitchEvent2DB




mean :: Floating a => [a] -> a
mean x = fst $ foldl' (\(!m,  !n) x -> (m+(x-m)/(n+1), n+1)) (0, 0) x


var :: (Fractional a, Floating a) => [a] -> a
var xs = Prelude.sum (map (\x -> (x - mu)^(2::Int)) xs)  / (n - 1)
    where mu = mean xs
          n = fromIntegral $ length xs

std :: (RealFloat a) => [a] -> a
std x = sqrt $ var x

