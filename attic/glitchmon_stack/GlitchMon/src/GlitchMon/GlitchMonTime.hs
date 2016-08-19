{-# LANGUAGE BangPatterns #-}

module GlitchMon.GlitchMonTime
( runGlitchMonTime
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
type Channel = String


runGlitchMonTime :: GP.GlitchParam
                 -> Channel
                 -> FilePath
                 -> IO()
--                 -> IO FilePath
runGlitchMonTime param chname cachefile = source cachefile $$ sink param chname


source :: FilePath
       -> Source IO (Int,Double)
source f = do
  gpslist' <- return $ readFile f >>= \x -> return $ lines x
  let gpslist = map ((\x->(read (head x) :: Int,read (x!!1) :: Double)).words) (unsafePerformIO gpslist')
  CL.sourceList gpslist


sink :: GP.GlitchParam
     -> Channel
     -> Sink (Int,Double) IO ()
sink param chname = do
  c <- await
  case c of
    Nothing -> sink param chname
    Just (gps, dt') -> do
      let dt = truncate dt'
          n = 0
      maybewave <- liftIO $ kagraWaveDataGet gps dt chname
      case maybewave of
        Nothing -> sink param chname
        Just wave -> do let param' = GP.updateGlitchParam'channel param chname
                            fs = GP.samplingFrequency param'
                            fsorig = samplingFrequency wave
                        if (fs /= fsorig)
                          then do let wave' = downsampleWaveData fs wave
                                      dataGps = (gps, n)
                                      param'2 = GP.updateGlitchParam'cgps param' (Just dataGps)
                                  fileRun' wave' param'2
                                  let s = unsafePerformIO $ timeRun chname wave' param'2
                                  sink s chname
                          else do let dataGps = (gps, n)
                                      param'2 = GP.updateGlitchParam'cgps param' (Just dataGps)
                                  fileRun' wave param'2
                                  let s = unsafePerformIO $ timeRun chname wave param'2
                                  sink s chname


fileRun' w param = do
   let dataGps = deformatGPS $ fromJust $ GP.cgps param
       param' = GP.updateGlitchParam'refwave param (takeWaveData (GP.chunklen param) w)
   liftIO $ print dataGps
--   liftIO $ glitchMon param' w


fileRun w param = do
   let dataGps = deformatGPS $ fromJust $ GP.cgps param
       param' = GP.updateGlitchParam'refwave param (takeWaveData (GP.chunklen param) w)
   liftIO $ glitchMon param' w


timeRun :: Channel
        -> WaveData
        -> GP.GlitchParam
        -> IO GP.GlitchParam
timeRun chname w param' = do
  let maybegps = GP.cgps param'
  case maybegps of
    Nothing -> do
      error "cgps not found. something wrong. please check it out."
    Just strtGps -> do
          let cdfp = GP.cdfparameter param'
              seglen = GP.segmentLength param'
          maybecdlist <- liftIO $ cleanDataFinder cdfp chname (formatGPS (deformatGPS strtGps +seglen), seglen)
          case maybecdlist of
            Nothing -> error "no clean data in the given gps interval"
            Just cdlist -> do
              let cdgps' = fst . last $ [(t,b)|(t,b)<-cdlist,b==True]
                  cdgps = fst cdgps'
                  param'2 = GP.updateGlitchParam'cgps param' (Just cdgps')
              maybew <- liftIO $ kagraWaveDataGet cdgps (GP.chunklen param'2) chname
              let param'3 = GP.updateGlitchParam'refwave param'2 (fromJust maybew)
              glitchMon param'3 w

glitchMon :: GP.GlitchParam
          -> WaveData
          -> IO GP.GlitchParam
glitchMon param w =
  runStateT (part'DataConditioning w) param >>= \(a, s) ->
    runStateT (part'EventTriggerGeneration a) s >>= \(a', s') ->
      runStateT (part'ParameterEstimation a') s' >>= \(a'', s'') ->
         case a'' of
--           Just t -> part'RegisterEventtoDB t >> return s''
           Just t -> do print "finishing glitchmon"
                        return s''
           Nothing -> do print "finishing glitchmon"
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


