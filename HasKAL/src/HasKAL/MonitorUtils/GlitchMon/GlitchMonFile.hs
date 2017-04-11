{-# LANGUAGE BangPatterns #-}

module HasKAL.MonitorUtils.GlitchMon.GlitchMonFile
( runGlitchMonFile
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

import qualified HasKAL.PlotUtils.HROOT.PlotGraph3D as H3
import qualified HasKAL.PlotUtils.HROOT.PlotGraph as H
import HasKAL.SpectrumUtils.SpectrumUtils (gwOnesidedPSDWaveData, gwspectrogramWaveData)

{--------------------
- Main Functions    -
--------------------}


runGlitchMonFile :: GP.GlitchParam
                 -> String
                 -> FilePath
                 -> IO()
--                 -> IO FilePath
runGlitchMonFile param chname fname = yield fname $$ sink param chname


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
        Nothing -> sink param chname
        Just (s, n, dt') -> do
          maybewave <- liftIO $ readFrameWaveData' General chname fname
          case maybewave of
            Nothing -> sink param chname
            Just wave -> do let param' = GP.updateGlitchParam'channel param chname
                                fs = GP.samplingFrequency param'
                                fsorig = samplingFrequency wave
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

                                      s <- fileRun wave' param'2
                                      sink s chname
                              else do let dataGps = (s, n)
                                          param'2 = GP.updateGlitchParam'cgps param' (Just dataGps)
                                      s <- fileRun wave param'2
                                      sink s chname


fileRun w param = do
   let dataGps = deformatGPS $ fromJust $ GP.cgps param
       traindatlen = GP.traindatlen param ::Double
       fs = GP.samplingFrequency param ::Double
       param' = GP.updateGlitchParam'refwave param (takeWaveData (floor (traindatlen*fs)) w)
   liftIO $ glitchMon param' w


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
