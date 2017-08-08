{-# LANGUAGE BangPatterns #-}

module HasKAL.MonitorUtils.GlitchMon.GlitchMonDAT
( runGlitchMonDAT
, runGlitchMonDATSQLite3
, runDataConditioningDAT
, runEventTriggerGenerationDAT
, eventDisplay
, eventDisplayF
)
where


import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad ((>>=), mapM_)
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
import Data.Int (Int32)
import Data.List (nub, foldl', elemIndices, maximum, minimum, lookup)
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.Text ( pack )
import qualified Data.Vector.Storable as VS
import Filesystem.Path (extension, (</>))
import Filesystem.Path.CurrentOS (decodeString, encodeString)
import HasKAL.DataBaseUtils.FrameFull.Data
import HasKAL.DataBaseUtils.FrameFull.Function (cleanDataFinder, kagraWaveDataGet)
import HasKAL.DetectorUtils.Detector(Detector(..))
import HasKAL.FrameUtils.FrameUtils (getGPSTime)
import HasKAL.FrameUtils.Function (readFrameWaveData')
import HasKAL.MathUtils.FFTW (dct2d, idct2d)
import qualified HasKAL.MonitorUtils.GlitchMon.GlitchParam as GP
import HasKAL.MonitorUtils.GlitchMon.PipelineFunction
import HasKAL.MonitorUtils.GlitchMon.Data (TrigParam (..))
import HasKAL.MonitorUtils.GlitchMon.RegisterGlitchEvent (registGlitchEvent2DB)
import HasKAL.MonitorUtils.GlitchMon.RegisterGlitchEventSQLite3 (registGlitchEvent2DBSQLite3)
import HasKAL.MonitorUtils.GlitchMon.Signature
import HasKAL.MonitorUtils.GlitchMon.DataConditioning
import HasKAL.MonitorUtils.GlitchMon.EventTriggerGeneration
import HasKAL.MonitorUtils.GlitchMon.ParameterEstimation
import HasKAL.MonitorUtils.GlitchMon.RegisterEventtoDB
import qualified HasKAL.PlotUtils.HROOT.PlotGraph3D as H3
import qualified HasKAL.PlotUtils.HROOT.PlotGraph as H
import HasKAL.SpectrumUtils.Function (updateMatrixElement, updateSpectrogramSpec)
import HasKAL.SpectrumUtils.Signature (Spectrum, Spectrogram)
import HasKAL.SpectrumUtils.SpectrumUtils (gwpsdV, gwOnesidedPSDV,gwOnesidedPSDWaveData, gwspectrogramWaveData)
import HasKAL.SignalProcessingUtils.LinearPrediction (lpefCoeffV, whiteningWaveData)
import HasKAL.SignalProcessingUtils.Resampling (downsampleWaveData)
import HasKAL.TimeUtils.Function (formatGPS, deformatGPS)
import HasKAL.TimeUtils.GPSfunction (getCurrentGps)
import HasKAL.TimeUtils.Signature (GPSTIME)
import qualified HasKAL.WaveUtils.Data as WD
import HasKAL.WaveUtils.Data hiding (detector, mean)
import HasKAL.WaveUtils.Signature

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



{--------------------
- Main Functions    -
--------------------}


runGlitchMonDAT  :: GP.GlitchParam
                 -> String
                 -> WaveData
                 -> IO()
--                 -> IO FilePath
runGlitchMonDAT param chname wave = yield wave $$ sink fileRun param chname


runGlitchMonDATSQLite3  :: GP.GlitchParam
                        -> String
                        -> WaveData
                        -> IO()
--                 -> IO FilePath
runGlitchMonDATSQLite3 param chname wave = yield wave $$ sink fileRunSQLite3 param chname


runDataConditioningDAT  :: GP.GlitchParam
                        -> String
                        -> WaveData
                        -> IO (Maybe WaveData)
--                 -> IO FilePath
runDataConditioningDAT param chname wave = yield wave $$ sinkDC dcRun param chname


runEventTriggerGenerationDAT  :: GP.GlitchParam
                              -> String
                              -> WaveData
                              -> IO (Maybe (Spectrogram, [[(Tile,ID)]]))
--                 -> IO FilePath
runEventTriggerGenerationDAT param chname wave = yield wave $$ sinkETG etgRun param chname


sink :: (WaveData -> GP.GlitchParam -> IO GP.GlitchParam)
     -> GP.GlitchParam
     -> String
     -> Sink WaveData IO ()
sink func param chname = do
  c <- await
  case c of
    Nothing -> return ()
    Just w -> do
      let gps = startGPSTime w
      let maybegps = Just (fst gps, snd gps, 0.0)
      case maybegps of
        Nothing -> sink func param chname
        Just (s, n, dt') -> do
          let param' = GP.updateGlitchParam'channel param chname
          let fs = GP.samplingFrequency param'
          let maybewave = Just w
          case maybewave of
            Nothing -> sink func param chname
            Just wave -> do let fsorig = samplingFrequency wave
                            if (fs /= fsorig)
                              then do liftIO $ print "start downsampling" >> hFlush stdout
                                      let wave' = downsampleWaveData fs wave
                                          dataGps = (s, n)
                                          param'2 = GP.updateGlitchParam'cgps param' (Just dataGps)
                                      case GP.DS `elem` GP.debugmode param of
                                         True -> do
                                           let dir = GP.debugDir param
                                           liftIO $ H3.spectrogramM H3.LogY
                                            H3.COLZ
                                            "mag"
                                            "spectrogram of downsampled timeseries"
                                            (dir++"/ds_sprcgram.png")
                                            ((0, 0), (0, 0))
                                            $ gwspectrogramWaveData 0.19 0.2 wave'

                                           liftIO $ H.plot H.Linear
                                            H.Line
                                            1
                                            H.RED
                                            ("time","amplitude")
                                            0.05
                                            "downsampled data"
                                            (dir++"/ds_timeseries.png")
                                            ((0,0),(0,0))
                                            $ zip [0,1/fs..] (NL.toList $ gwdata wave')

                                           liftIO $ H.plotV H.LogXY
                                            H.Line
                                            1
                                            H.RED
                                            ("frequency [Hz]","ASD [Hz^-1/2]")
                                            0.05
                                            "spectrum of downsampled timeseries"
                                            (dir++"/ds_spectrum.png")
                                            ((0,0),(0,0))
                                            $ gwOnesidedPSDWaveData 1 wave
                                         False -> liftIO $ Prelude.return ()

                                      s <- liftIO $ func wave' param'2
                                      return ()
                                      --sink func s chname
                              else do let dataGps = (s, n)
                                          param'2 = GP.updateGlitchParam'cgps param' (Just dataGps)
                                      s <- liftIO $ func wave param'2
                                      return ()
                                      --sink func s chname


sinkDC :: (WaveData -> GP.GlitchParam -> IO (WaveData, GP.GlitchParam))
       -> GP.GlitchParam
       -> String
       -> Sink WaveData IO (Maybe WaveData)
sinkDC func param chname = do
  c <- await
  case c of
    Nothing -> return Nothing
    Just w -> do
      let gps = startGPSTime w
      let maybegps = Just (fst gps, snd gps, 0.0)
      case maybegps of
        Nothing -> sinkDC func param chname
        Just (s, n, dt') -> do
          let param' = GP.updateGlitchParam'channel param chname
          let fs = GP.samplingFrequency param'
          let maybewave = Just w
          case maybewave of
            Nothing -> sinkDC func param chname
            Just wave -> do let fsorig = samplingFrequency wave
                            if (fs /= fsorig)
                              then do liftIO $ print "start downsampling" >> hFlush stdout
                                      let wave' = downsampleWaveData fs wave
                                          dataGps = (s, n)
                                          param'2 = GP.updateGlitchParam'cgps param' (Just dataGps)
                                      case GP.DS `elem` GP.debugmode param of
                                         True -> do
                                           let dir = GP.debugDir param
                                           liftIO $ H3.spectrogramM H3.LogY
                                            H3.COLZ
                                            "mag"
                                            "spectrogram of downsampled timeseries"
                                            (dir++"/ds_sprcgram.png")
                                            ((0, 0), (0, 0))
                                            $ gwspectrogramWaveData 0.19 0.2 wave'

                                           liftIO $ H.plot H.Linear
                                            H.Line
                                            1
                                            H.RED
                                            ("time","amplitude")
                                            0.05
                                            "downsampled data"
                                            (dir++"/ds_timeseries.png")
                                            ((0,0),(0,0))
                                            $ zip [0,1/fs..] (NL.toList $ gwdata wave')

                                           liftIO $ H.plotV H.LogXY
                                            H.Line
                                            1
                                            H.RED
                                            ("frequency [Hz]","ASD [Hz^-1/2]")
                                            0.05
                                            "spectrum of downsampled timeseries"
                                            (dir++"/ds_spectrum.png")
                                            ((0,0),(0,0))
                                            $ gwOnesidedPSDWaveData 1 wave
                                         False -> liftIO $ Prelude.return ()

                                      (a,s) <- liftIO $ func wave' param'2
                                      return (Just a)
--                                      sinkDC func s chname
                              else do let dataGps = (s, n)
                                          param'2 = GP.updateGlitchParam'cgps param' (Just dataGps)
                                      (a,s) <- liftIO $ func wave param'2
                                      return (Just a)
--                                      sinkDC func s chname


sinkETG :: (WaveData -> GP.GlitchParam -> IO ((Spectrogram, [[(Tile,ID)]]), GP.GlitchParam))
        -> GP.GlitchParam
        -> String
        -> Sink WaveData IO (Maybe (Spectrogram, [[(Tile,ID)]]))
sinkETG func param chname = do
  c <- await
  case c of
    Nothing -> return Nothing
    Just w -> do
      let gps = startGPSTime w
      let maybegps = Just (fst gps, snd gps, 0.0)
      case maybegps of
        Nothing -> sinkETG func param chname
        Just (s, n, dt') -> do
          let param' = GP.updateGlitchParam'channel param chname
          let fs = GP.samplingFrequency param'
          let maybewave = Just w
          case maybewave of
            Nothing -> sinkETG func param chname
            Just wave -> do let fsorig = samplingFrequency wave
                            if (fs /= fsorig)
                              then do liftIO $ print "start downsampling" >> hFlush stdout
                                      let wave' = downsampleWaveData fs wave
                                          dataGps = (s, n)
                                          param'2 = GP.updateGlitchParam'cgps param' (Just dataGps)
                                      case GP.DS `elem` GP.debugmode param of
                                         True -> do
                                           let dir = GP.debugDir param
                                           liftIO $ H3.spectrogramM H3.LogY
                                            H3.COLZ
                                            "mag"
                                            "spectrogram of downsampled timeseries"
                                            (dir++"/ds_sprcgram.png")
                                            ((0, 0), (0, 0))
                                            $ gwspectrogramWaveData 0.19 0.2 wave'

                                           liftIO $ H.plot H.Linear
                                            H.Line
                                            1
                                            H.RED
                                            ("time","amplitude")
                                            0.05
                                            "downsampled data"
                                            (dir++"/ds_timeseries.png")
                                            ((0,0),(0,0))
                                            $ zip [0,1/fs..] (NL.toList $ gwdata wave')

                                           liftIO $ H.plotV H.LogXY
                                            H.Line
                                            1
                                            H.RED
                                            ("frequency [Hz]","ASD [Hz^-1/2]")
                                            0.05
                                            "spectrum of downsampled timeseries"
                                            (dir++"/ds_spectrum.png")
                                            ((0,0),(0,0))
                                            $ gwOnesidedPSDWaveData 1 wave
                                         False -> liftIO $ Prelude.return ()

                                      (a,s) <- liftIO $ func wave' param'2
                                      return (Just a)
                                      --sinkETG func s chname
                              else do let dataGps = (s, n)
                                          param'2 = GP.updateGlitchParam'cgps param' (Just dataGps)
                                      (a,s) <- liftIO $ func wave param'2
                                      return (Just a)
                                      --sinkETG func s chname


fileRun :: WaveData
        -> GP.GlitchParam
        -> IO GP.GlitchParam
        --        -> Sink (Double,VS.Vector Double) IO GP.GlitchParam
fileRun w param = do
   let dataGps = deformatGPS $ fromJust $ GP.cgps param
       traindatlen = GP.traindatlen param ::Double
       fs = GP.samplingFrequency param ::Double
       param' = GP.updateGlitchParam'refwave param (takeWaveData (floor (traindatlen*fs)) w)
   glitchMon param' w


fileRunSQLite3 :: WaveData
               -> GP.GlitchParam
               -> IO GP.GlitchParam
        --        -> Sink (Double,VS.Vector Double) IO GP.GlitchParam
fileRunSQLite3 w param = do
   let dataGps = deformatGPS $ fromJust $ GP.cgps param
       traindatlen = GP.traindatlen param ::Double
       fs = GP.samplingFrequency param ::Double
       param' = GP.updateGlitchParam'refwave param (takeWaveData (floor (traindatlen*fs)) w)
   glitchMonSQLite3 param' w


dcRun :: WaveData
      -> GP.GlitchParam
      -> IO (WaveData, GP.GlitchParam)
        --        -> Sink (Double,VS.Vector Double) IO GP.GlitchParam
dcRun w param = do
   let dataGps = deformatGPS $ fromJust $ GP.cgps param
       traindatlen = GP.traindatlen param ::Double
       fs = GP.samplingFrequency param ::Double
       param' = GP.updateGlitchParam'refwave param (takeWaveData (floor (traindatlen*fs)) w)
   dataConditioning param' w


etgRun :: WaveData
       -> GP.GlitchParam
       -> IO ((Spectrogram, [[(Tile,ID)]]), GP.GlitchParam)
etgRun w param = do
   let dataGps = deformatGPS $ fromJust $ GP.cgps param
       traindatlen = GP.traindatlen param ::Double
       fs = GP.samplingFrequency param ::Double
       param' = GP.updateGlitchParam'refwave param (takeWaveData (floor (traindatlen*fs)) w)
   eventTriggerGeneration param' w


glitchMon :: GP.GlitchParam
          -> WaveData
          -> IO GP.GlitchParam
glitchMon param w =
  runStateT (part'DataConditioning w) param >>= \(a, s) ->
    runStateT (part'EventTriggerGeneration a) s >>= \(a', s') ->
      runStateT (part'ParameterEstimation a') s' >>= \(a'', s'') ->
         case a'' of
           Just t -> do -- part'RegisterEventtoDB t
                        print "finishing glitchmon" >> return s''
           Nothing -> do print "No event from glitchmon"
                         return s''


glitchMonSQLite3 :: GP.GlitchParam
                 -> WaveData
                 -> IO GP.GlitchParam
glitchMonSQLite3 param w =
  runStateT (part'DataConditioning w) param >>= \(a, s) ->
    runStateT (part'EventTriggerGeneration a) s >>= \(a', s') ->
      runStateT (part'ParameterEstimation a') s' >>= \(a'', s'') ->
         case a'' of
           Just t -> do part'RegisterEventtoDBSQLite3 t
                        print "finishing glitchmon" >> return s''
           Nothing -> do print "No event from glitchmon"
                         return s''


dataConditioning :: GP.GlitchParam
                 -> WaveData
                 -> IO (WaveData, GP.GlitchParam)
dataConditioning param w =
  runStateT (part'DataConditioning' w) param >>= \(a, s) ->
    do --print $ take 5 $ VS.toList (gwdata a)
       return (a,s)


eventTriggerGeneration :: GP.GlitchParam
                       -> WaveData
                       -> IO ((Spectrogram, [[(Tile,ID)]]), GP.GlitchParam)
eventTriggerGeneration param w =
  runStateT (part'DataConditioning w) param >>= \(a, s) ->
    runStateT (part'EventTriggerGeneration a) s >>= \(a', s') ->
      return (a', s')


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


{- internal functions -}

chekingFile path = takeExtension path `elem` [".gwf"] && head (takeFileName path) /= '.'


getAbsPath dir1 dir2 = encodeString $ decodeString dir1 </> decodeString dir2


breakTime margin = unsafePerformIO $ do
  dt <- getCurrentGps >>= \gps-> return $ timeToNextDir gps
  return $ (dt+margin)*1000000


getCurrentDir :: String -> String
getCurrentDir gps = take 5 gps


getNextDir :: String -> String
getNextDir gps =
  let gpsHead' = take 5 gps
      gpsHead = read gpsHead' :: Int
   in take 5 $ show (gpsHead+1)


timeToNextDir :: String -> Int
timeToNextDir gps =
  let currentGps = read gps :: Int
      (gpsHead', gpsTail') = (take 5 gps, drop 5 gps)
      gpsHead = read gpsHead' :: Int
      gpsTail = replicate (length gpsTail') '0'
      nextGps = read (show (gpsHead+1) ++ gpsTail) :: Int
   in nextGps - currentGps


gowatch dname f g =  do b <- liftIO $ doesDirectoryExist dname
                        case b of
                          False -> do gps <- liftIO getCurrentGps
                                      let cdir' = getCurrentDir gps
                                          cdir  = drop (length dname -5) dname
                                      case cdir' > cdir of
                                        True -> do let dname' = (take (length dname -5) dname)++cdir'
                                                   liftIO $ threadDelay 1000000
                                                   gowatch dname' (g cdir') g
                                        False -> do liftIO $ threadDelay 1000000
                                                    gowatch dname f g
                          True  -> f
