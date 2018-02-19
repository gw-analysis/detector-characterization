{-# LANGUAGE BangPatterns #-}

module HasKAL.MonitorUtils.GlitchMon.GlitchMonRT
( runGlitchMonRT
, eventDisplay
, eventDisplayF
)
where


import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Lens
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad ((>>=), mapM_, void)
import Control.Monad.State ( StateT
                           , runStateT
                           , execStateT
                           , get
                           , put
                           , liftIO
                           )
import Data.Conduit ( bracketP
                    , yield
                    , await
                    , ($$)
                    , Source
                    , Sink
                    , Conduit
                    )
import qualified Data.Conduit.List as CL
import Data.Default
import Data.Int (Int32)
import Data.List (nub, foldl', elemIndices, maximum, minimum, lookup)
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.Text ( pack )
import Filesystem.Path (extension, (</>))
import Filesystem.Path.CurrentOS (decodeString, encodeString)
import HasKAL.DataBaseUtils.FrameFull.Data
import HasKAL.DataBaseUtils.FrameFull.Function (cleanDataFinder, kagraWaveDataGet)
import HasKAL.DetectorUtils.Detector(Detector(..))
import HasKAL.FrameUtils.FrameUtils (getGPSTime)
import HasKAL.FrameUtils.Function (readFrameWaveData')
import HasKAL.MathUtils.FFTW (dct2d, idct2d)
import HasKAL.SpectrumUtils.Function (updateMatrixElement, updateSpectrogramSpec)
import HasKAL.SpectrumUtils.Signature (Spectrum, Spectrogram)
import HasKAL.SpectrumUtils.SpectrumUtils (gwpsdV, gwOnesidedPSDV)
import HasKAL.SignalProcessingUtils.LinearPrediction (lpefCoeffV, whiteningWaveData)
import HasKAL.SignalProcessingUtils.Resampling (downsampleWaveData)
import HasKAL.TimeUtils.Function (formatGPS, deformatGPS)
import HasKAL.TimeUtils.GPSfunction (getCurrentGps)
import HasKAL.TimeUtils.Signature (GPSTIME)
import qualified HasKAL.WaveUtils.Data as WD
import HasKAL.WaveUtils.Data hiding (detector, mean)
import HasKAL.WaveUtils.Signature
import Network.NDS2 as NDS2 hiding (dataType)
import Network.NDS2.Conduit (ndsSource)
import Numeric.LinearAlgebra as NL
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath.Posix (takeExtension, takeFileName)
import System.FSNotify ( Debounce(..)
                       , Event(..)
                       , WatchConfig(..)
                       , withManagerConf
                       , watchDir
                       , eventPath
                       )
import System.IO (hFlush, stdout)
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout.Lifted (timeout)

import qualified HasKAL.MonitorUtils.GlitchMon.GlitchParam as GP
import HasKAL.MonitorUtils.GlitchMon.PipelineFunction
import HasKAL.MonitorUtils.GlitchMon.Data (TrigParam (..))
import HasKAL.MonitorUtils.GlitchMon.RegisterGlitchEvent (registGlitchEvent2DB)
import HasKAL.MonitorUtils.GlitchMon.Signature
import HasKAL.MonitorUtils.GlitchMon.DataConditioning
import HasKAL.MonitorUtils.GlitchMon.EventTriggerGeneration
import HasKAL.MonitorUtils.GlitchMon.ParameterEstimation
import HasKAL.MonitorUtils.GlitchMon.RegisterEventtoDB



{--------------------
- Main Functions    -
--------------------}


runGlitchMonRT :: ConnectParams -> GP.GlitchParam -> String -> IO ()
runGlitchMonRT connParams param chname = do
  ndsSource connParams (def & channelNames .~ [chname]) $$ sink param chname


-- | Convert a NDS2 Buffer to WaveData format.
nds2BufferToWaveData :: NDS2.Buffer -> WaveData
nds2BufferToWaveData buf =
  WaveData { WD.detector       = KAGRA -- TODO: Remove hardcoded detector
           , dataType          = buf^.channelInfo.name
           , samplingFrequency = realToFrac $ buf^.channelInfo.sampleRate
           , startGPSTime      = (buf^.startGpsSecond, buf^.startGpsNanosecond) 
           , stopGPSTime       = (buf^.stopGpsSecond, buf^.startGpsNanosecond) 
           , gwdata            = buf^.timeSeries
           }

sink :: GP.GlitchParam
     -> String
     -> Sink [NDS2.Buffer] IO ()
sink param chname = do
  maybeBuffers <- await
  case maybeBuffers of
    Nothing -> return ()
    Just [buf] -> do
      let wave = nds2BufferToWaveData $ buf
          param' = GP.updateGlitchParam'channel param chname
          fs = GP.samplingFrequency param'
          fsorig = samplingFrequency wave
      if (fs /= fsorig)
        then do let wave' = downsampleWaveData fs wave
                void $ go wave' param'
        else do s <- go wave param'
                sink s chname
    Just bufs -> error $ (show $ length bufs) ++ " buffers were received instead of 1"
  where 
    go w param' = do
      let maybegps = GP.cgps param'
      case maybegps of
        Nothing -> do
          currGps' <- liftIO $ getCurrentGps
          let currGps = formatGPS (read currGps')
              param'2 = GP.updateGlitchParam'cgps param' (Just currGps)
              chunklen = fromIntegral $ GP.segmentLength param'2
              fs = GP.samplingFrequency param'2
              param'3 = GP.updateGlitchParam'refwave param'2 (takeWaveData (floor (chunklen*fs)) w)
          liftIO $ glitchMon param'3 w
        Just gpsold -> do
          currGps' <- liftIO $ getCurrentGps
          let currGps = formatGPS (read currGps')
              difft = (deformatGPS currGps) - (deformatGPS gpsold)
              cdfIntvl = GP.cdfInterval param' 
          case difft >= (fromIntegral cdfIntvl) of  -- ^ clean data update every 10 minutes
            True -> do
              let cdfp = GP.cdfparameter param'
              maybecdlist <- liftIO $ cleanDataFinder cdfp chname (currGps, 600.0)
              case maybecdlist of
                Nothing -> error "no clean data in the given gps interval"
                Just cdlist -> do
                  let cdgps' = fst . last $ [(t,b)|(t,b)<-cdlist,b==True]
                      cdgps = fst cdgps'
                      param'2 = GP.updateGlitchParam'cgps param' (Just cdgps')
                      chunklen = fromIntegral $ GP.segmentLength param'2
                      fs = GP.samplingFrequency param'2
                  maybew <- liftIO $ kagraWaveDataGet cdgps (floor (chunklen*fs)) chname
                  let param'3 = GP.updateGlitchParam'refwave param'2 (fromJust maybew)
                  liftIO $ glitchMon param'3 w
            False -> liftIO $ glitchMon param' w


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
             -> IO (Maybe [(TrigParam,ID)], GP.GlitchParam, (Spectrogram, [[(Tile,ID)]]))
eventDisplay param w =
  runStateT (part'DataConditioning w) param >>= \(a, s) ->
    runStateT (part'EventTriggerGeneration a) s >>= \(a', s') ->
       do (trigparam, param') <- runStateT (part'ParameterEstimation a') s'
          return (trigparam, param',a')


eventDisplayF :: GP.GlitchParam
              -> FilePath
              -> String
              -> IO (Maybe [(TrigParam,ID)], GP.GlitchParam, (Spectrogram, [[(Tile,ID)]]))
eventDisplayF param fname chname = do
  maybegps <- getGPSTime fname
  case maybegps of
    Nothing -> error "file broken"
    Just (s, n, dt') -> do
      maybewave <- readFrameWaveData' General chname fname
      case maybewave of
        Nothing -> error "file broken"
        Just w -> runStateT (part'DataConditioning w) param >>= \(a, s) ->
                    runStateT (part'EventTriggerGeneration a) s >>= \(a', s') ->
                       do (trigparam, param') <- runStateT (part'ParameterEstimation a') s'
                          return (trigparam, param',a')
