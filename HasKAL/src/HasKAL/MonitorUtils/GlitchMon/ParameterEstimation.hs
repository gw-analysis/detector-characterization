{-# LANGUAGE BangPatterns, FlexibleContexts, TypeFamilies #-}




module HasKAL.MonitorUtils.GlitchMon.ParameterEstimation
( part'ParameterEstimation
) where


import Control.Monad.State (StateT, runStateT, execStateT, get, put, liftIO)
import Data.Int (Int32)
import Data.Maybe (fromJust)
import HasKAL.SpectrumUtils.Signature (Spectrum, Spectrogram)
import HasKAL.TimeUtils.Function (formatGPS, deformatGPS)
import Numeric.LinearAlgebra as NL
import Numeric.LinearAlgebra (atIndex)

import HasKAL.MonitorUtils.GlitchMon.Data (TrigParam (..))
import qualified HasKAL.MonitorUtils.GlitchMon.GlitchParam as GP
import HasKAL.MonitorUtils.GlitchMon.Signature
import System.IO (hFlush, stdout)



part'ParameterEstimation :: (Spectrogram, [[(Tile,ID)]])
                         -> StateT GP.GlitchParam IO (Maybe [(TrigParam,ID)])
part'ParameterEstimation (m,ids) = do
  liftIO $ print "start parameter estimation." >> hFlush stdout
  param <- get
  let fs = GP.samplingFrequency param
   in getParam param (m,ids)
  where
    getParam param (m,ids) = do
      let (trigT, trigF, trigM) = m
          mrow = NL.rows trigM
          mcol = NL.cols trigM
          zerom = (mrow >< mcol) (replicate (mrow*mcol) (0::Double))
          nfreq = (floor $ GP.nfrequency param * fs) :: Int
          ntime = (floor $ GP.ntimeSlide param * fs) :: Int
          fs = GP.samplingFrequency param
      case (trigM == zerom) of
        True -> return Nothing
        False-> return $ Just $ (flip map) ids $
          \island-> do
            let (tile,tag) = unzip island
                nsize = fromIntegral $ length island :: Int32
                tmin = formatGPS $ trigT `atIndex` (minimum . snd . unzip $ tile)
                tmax' = trigT `atIndex` (maximum . snd . unzip $ tile)
                tmax = formatGPS $ tmax' + fromIntegral nfreq/fs
                fmin = trigF `atIndex` (minimum . fst . unzip $ tile)
                fmax' = trigF `atIndex` (maximum . fst . unzip $ tile)
                fmax = fmax' + fs/fromIntegral nfreq
                xx = map (\i->trigM `atIndex` i) tile :: [Double]
                (blackpower,maxid) = maximum' xx :: (Double,Int)
                blackt = formatGPS $ trigT `atIndex` (snd $ tile!!maxid)
                blackf = trigF `atIndex` (fst $ tile!!maxid)
                tfs = fromIntegral $ (floor fs::Int) :: Int32
             in (TrigParam { detector = Just "General"
                           , event_gpsstarts = Just (fromIntegral . fst $ tmin)
                           , event_gpsstartn = Just (fromIntegral . snd $ tmin)
                           , event_gpsstops  = Just (fromIntegral . fst $ tmax)
                           , event_gpsstopn  = Just (fromIntegral . snd $ tmax)
                           , event_fmin = Just fmin
                           , event_fmax = Just fmax
                           , event_cgpss = Just (fromIntegral . fst $ blackt)
                           , event_cgpsn = Just (fromIntegral . snd $ blackt)
                           , duration = Just $ deformatGPS tmax - deformatGPS tmin
                           , energy = Nothing
                           , island_size = Just nsize
                           , central_frequency = Just blackf
                           , snr = Just blackpower
                           , significance = Nothing
                           , latitude = Nothing
                           , longitude = Nothing
                           , channel = Just (GP.channel param)
                           , sampling_rate = Just tfs
                           , segment_gpsstarts = Nothing
                           , segment_gpsstartn = Nothing
                           , segment_gpsstops = Nothing
                           , segment_gpsstopn = Nothing
                           , dq_flag = Nothing
                           , pipeline = Just "iKAGRA Glitch pipeline"
                           }
                 , head tag)
            where
              maximum' x = let maxx = maximum x :: Double
                            in (maxx, fromJust $ lookup maxx (zip x [0..]))


