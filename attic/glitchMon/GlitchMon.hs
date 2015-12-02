
module GlitchMon.GlitchMon
( runGlitchMon
, eventDisplay
, eventDisplayF
)
where


import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad ((>>=), mapM_)
import Control.Monad.State (StateT, runStateT, execStateT, get, put, liftIO)
import Data.Conduit (bracketP, yield,  await, ($$), Source, Sink, Conduit)
import qualified Data.Conduit.List as CL
import Data.Int (Int32)
import Data.List (nub, foldl', elemIndices, maximum, minimum, lookup)
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.Text ( pack )
import Filesystem.Path (extension)
import Filesystem.Path.CurrentOS (decodeString,  encodeString)
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
import HasKAL.WaveUtils.Data hiding (detector, mean)
import HasKAL.WaveUtils.Signature
import Numeric.LinearAlgebra as NL
import System.FSNotify (Debounce(..), Event(..), WatchConfig(..), withManagerConf, watchTree, eventPath)
import System.IO (hFlush, stdout)

import qualified GlitchMon.GlitchParam as GP
import GlitchMon.PipelineFunction
import GlitchMon.Data (TrigParam (..))
import GlitchMon.RegisterGlitchEvent (registGlitchEvent2DB)
import GlitchMon.Signature
import GlitchMon.DataConditioning
import GlitchMon.EventTriggerGeneration
import GlitchMon.ParameterEstimation
import GlitchMon.RegisterEventtoDB


{--------------------
- Main Functions    -
--------------------}


runGlitchMon watchdir param chname =
  source watchdir $$ sink param chname


source :: FilePath
       -> Source IO FilePath
source watchdir = do
  let config = WatchConfig
                 { confDebounce = DebounceDefault
                 , confPollInterval = 20000000 -- 20seconds
                 , confUsePolling = True
                 }
  x <- liftIO $ withManagerConf config $ \manager -> do
    fname <- liftIO newEmptyMVar
    _ <- watchTree manager watchdir (const True)
      $ \event -> case event of
        Removed _ _ -> putStrLn "file removed" >> hFlush stdout
        _           -> case extension (decodeString $ eventPath event) of
                         Just ext -> if (ext==filepart)
                           then
                             putStrLn "file downloading" >> hFlush stdout
                           else if (ext==gwf)
                             then do
                               let gwfname = eventPath event
                               case length (elemIndices '.' gwfname) of
                                 1 -> putMVar fname gwfname
                                 _ -> putStrLn "file saving" >> hFlush stdout
                             else
                               putStrLn "file extension should be .filepart or .gwf" >> hFlush stdout
    takeMVar fname
  yield x >> source watchdir
  where filepart = pack "filepart"
        gwf = pack "gwf"


sink :: GP.GlitchParam
     -> String
     -> Sink String IO ()
sink param chname = do
  c <- await
  case c of
    Nothing -> sink param chname
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
                              then do let wave' = downsampleWaveData fs wave
                                      s <- liftIO $ glitchMon param' wave'
                                      sink s chname
                              else do s <- liftIO $ glitchMon param' wave
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




