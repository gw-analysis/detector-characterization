




module GlitchMon.ParameterEstimation
( part'ParameterEstimation
) where


import Control.Monad.State (StateT, runStateT, execStateT, get, put, liftIO)
import Data.Int (Int32)
import Data.Maybe (fromJust)
import HasKAL.SpectrumUtils.Signature (Spectrum, Spectrogram)
import HasKAL.TimeUtils.Function (formatGPS, deformatGPS)
import Numeric.LinearAlgebra as NL

import GlitchMon.Data (TrigParam (..))
import qualified GlitchMon.GlitchParam as GP
import GlitchMon.Signature


part'ParameterEstimation :: (Spectrogram, [[(Tile,ID)]])
                         -> StateT GP.GlitchParam IO (Maybe [(TrigParam,ID)])
part'ParameterEstimation (m,ids) = do
  liftIO $ print "start parameter estimation."
  param <- get
  let fs = GP.samplingFrequency param
   in getParam param (m,ids)
  where
    getParam param (m,ids) = do
      let (trigT, trigF, trigM) = m
          mrow = NL.rows trigM
          mcol = NL.cols trigM
          zerom = (mrow >< mcol) (replicate (mrow*mcol) (0::Double))
          nfreq = GP.nfrequency param
          ntime = GP.ntimeSlide param
          fs = GP.samplingFrequency param
      case (trigM == zerom) of
        True -> return Nothing
        False-> return $ Just $ (flip map) ids $
          \island-> do
            let (tile,tag) = unzip island
                tmin = formatGPS $ trigT @> (minimum . fst . unzip $ tile)
                tmax' = trigT @> (maximum . fst . unzip $ tile)
                tmax = formatGPS $ tmax' + fromIntegral nfreq/fs
                fmin = trigF @> (minimum . snd . unzip $ tile)
                fmax' = trigF @> (maximum . snd . unzip $ tile)
                fmax = fmax' + fs/fromIntegral nfreq
                xx = map (\i->trigM @@> i) tile :: [Double]
                (blackpower,maxid) = maximum' xx :: (Double,Int)
                blackt = formatGPS $ trigT @> (fst $ tile!!maxid)
                blackf = trigF @> (snd $ tile!!maxid)
                tfs = fromIntegral $ (floor fs::Int) :: Int32
             in (TrigParam { detector = Just "General"
                                    , event_gpsstarts = Just (fromIntegral . fst $ tmin)
                                    , event_gpsstartn = Just (fromIntegral . snd $ tmin)
                                    , event_gpsstops  = Just (fromIntegral . fst $ tmax)
                                    , event_gpsstopn  = Just (fromIntegral . snd $ tmax)
                                    , event_cgpss = Just (fromIntegral . fst $ blackt)
                                    , event_cgpsn = Just (fromIntegral . snd $ blackt)
                                    , duration = Just $ deformatGPS tmax - deformatGPS tmin
                                    , energy = Nothing
                                    , central_frequency = Just fs
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
                            in (maxx, fromJust $ lookup maxx (zip x [1..]))


part'ParameterEstimation' :: Spectrogram
                          -> StateT GP.GlitchParam IO (Maybe TrigParam)
part'ParameterEstimation' m = do
  param <- get
  let fs = GP.samplingFrequency param
  let (trigT, trigF, trigM) = m
      mrow = NL.rows trigM
      mcol = NL.cols trigM
      zerom = (mrow >< mcol) (replicate (mrow*mcol) (0::Double))
  case (trigM == zerom) of
    False -> do
      let indxBlack = maxIndex trigM
          tsnr = trigM @@> indxBlack
          gps = formatGPS $ trigT @> fst indxBlack
          gpss = fromIntegral $ fst gps :: Int32
          gpsn = fromIntegral $ snd gps :: Int32
          fc = trigF @> snd indxBlack
          tfs = fromIntegral $ truncate fs :: Int32
      return $ Just TrigParam { detector = Just "General"
                              , event_gpsstarts = Nothing
                              , event_gpsstartn = Nothing
                              , event_gpsstops  = Nothing
                              , event_gpsstopn  = Nothing
                              , event_cgpss = Just gpss
                              , event_cgpsn = Just gpsn
                              , duration = Nothing
                              , energy = Nothing
                              , central_frequency = Just fc
                              , snr = Just tsnr
                              , significance = Nothing
                              , latitude = Nothing
                              , longitude = Nothing
                              , channel = Nothing
                              , sampling_rate = Just tfs
                              , segment_gpsstarts = Nothing
                              , segment_gpsstartn = Nothing
                              , segment_gpsstops = Nothing
                              , segment_gpsstopn = Nothing
                              , dq_flag = Nothing
                              , pipeline = Just "iKAGRA Glitch pipeline"
                              }
    True -> return Nothing


