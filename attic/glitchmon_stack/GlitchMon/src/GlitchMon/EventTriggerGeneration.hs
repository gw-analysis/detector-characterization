
{-# LANGUAGE BangPatterns #-}

module GlitchMon.EventTriggerGeneration
( part'EventTriggerGeneration
) where



import Control.Monad.State (StateT, runStateT, execStateT, get, put, liftIO)
import Data.List (nub,  foldl',  elemIndices,  maximum,  minimum,  lookup)
import qualified Data.Set as Set
import HasKAL.MathUtils.FFTW (dct2d, idct2d)
import HasKAL.SignalProcessingUtils.Interpolation
import HasKAL.SpectrumUtils.Function (updateMatrixElement,  updateSpectrogramSpec)
import HasKAL.SpectrumUtils.Signature (Spectrum,  Spectrogram)
import HasKAL.SpectrumUtils.SpectrumUtils (gwpsdV, gwOnesidedPSDV)
import HasKAL.TimeUtils.Function (formatGPS,  deformatGPS)
import HasKAL.WaveUtils.Data hiding (detector, mean)
import Numeric.LinearAlgebra as NL
import qualified GlitchMon.GlitchParam as GP
import GlitchMon.PipelineFunction
import GlitchMon.Signature
import qualified HasKAL.PlotUtils.HROOT.PlotGraph3D as H3
import qualified HasKAL.PlotUtils.HROOT.PlotGraph as H

part'EventTriggerGeneration :: WaveData
                            -> StateT GP.GlitchParam IO (Spectrogram, [[(Tile,ID)]])
part'EventTriggerGeneration wave = do
  liftIO $ print "start event trigger generation"
  param <- get
  (a, s) <- liftIO $ runStateT (section'TimeFrequencyExpression wave) param
  section'Clustering (basePixel9 ,2) a


section'TimeFrequencyExpression :: WaveData
                                -> StateT GP.GlitchParam IO Spectrogram
section'TimeFrequencyExpression whnWaveData = do
  liftIO $ print "start time-frequency expansion"
  param <- get
  let refpsd = GP.refpsd param
      fs = GP.samplingFrequency param
      nfreq = GP.nfrequency param
      nfreq2 = nfreq `div` 2
      ntimeSlide = GP.ntimeSlide param
      ntime = ((NL.dim $ gwdata whnWaveData) - chunklen) `div` ntimeSlide
      chunklen = GP.chunklen param
      snrMatF = scale (fs/fromIntegral nfreq) $ fromList [0.0, 1.0..fromIntegral nfreq2-1]
      snrMatT = scale (fromIntegral ntimeSlide/fs) $ fromList [0.0, 1.0..fromIntegral ntime -1]
      snrMatT' = mapVector (+deformatGPS (startGPSTime whnWaveData)) snrMatT
      snrMatP = NL.trans $ NL.flipud $ (ntime><nfreq2) $ concatMap (take nfreq2 . toList . calcSpec) [0..ntime-1]
        where 
          calcSpec tindx = snd $ gwOnesidedPSDV (NL.subVector (ntimeSlide*tindx) nfreq (gwdata whnWaveData)) nfreq fs
      out = (snrMatT', snrMatF, snrMatP)
  liftIO $ H3.spectrogramM H3.LogY
                           H3.COLZ
                           "mag"
                           "pixelSNR spectrogram"
                           "production/gw150914_pixelSNR_spectrogram.png"
                           ((0, 0), (20, 400))
                           out
  return out


section'Clustering :: ((Int, Int) -> [(Int, Int)],Int)
                   -> Spectrogram
                   -> StateT GP.GlitchParam IO (Spectrogram,[[(Tile,ID)]])
section'Clustering (cfun,minN) (snrMatT, snrMatF, snrMatP') = do
  liftIO $ print "start seedless clustering"
  param <- get
  let l = NL.toList $ NL.flatten snrMatP'
      l' = (NL.dim snrMatF><NL.dim snrMatT) l
      dcted' = dct2d l'
      ncol = cols dcted'
      nrow = rows dcted'
      zeroElementc = [(x, y) | x<-[0..nrow-1], y<-[ncol-GP.resolvTime param..ncol-1]]
      zeroElementr = [(x, y) | y<-[0..ncol-1], x<-[nrow-GP.resolvFreq param..nrow-1]]
      zeroElement = zeroElementr ++ zeroElementc
  liftIO $ print $ ncol
  liftIO $ print $ nrow
  let dcted = updateMatrixElement dcted' zeroElement $ take (length zeroElement) [0, 0..]
      snrMatP = idct2d dcted
  let thresIndex = head $ NL.find (>=GP.cutoffFreq param) snrMatF
      snrMat = (snrMatT, NL.subVector thresIndex (nrow-thresIndex-1) snrMatF, NL.dropRows thresIndex snrMatP)
      (tt, ff, mg) = snrMat
--      thrsed = NL.find (<=GP.clusterThres param) mg
      thrsed = NL.find (>=GP.clusterThres param) mg
      survivor = nub' $ excludeOnePixelIsland cfun thrsed
  liftIO $ print $ length survivor
  liftIO $ print $ survivor
  let survivorwID = taggingIsland cfun minN survivor
      zeroMatrix = (nrow><ncol) $ replicate (ncol*nrow) 0.0
      survivorValues = map (\x->mg@@>x) survivor
      newM = updateSpectrogramSpec snrMat
       $ updateMatrixElement zeroMatrix survivor survivorValues
  liftIO $ H3.spectrogramM H3.LogY
                           H3.COLZ
                           "mag"
                           "clustered PixelSNR spectrogram"
                           "production/gw150914_cluster_spectrogram.png"
                           ((0, 0), (20, 400))
                           newM              
 
  liftIO $ print "finishing ETG section.."
  liftIO $ print survivorwID
  return (newM, survivorwID)



-- | O (nlog n) nub 
-- | lent from http://d.hatena.ne.jp/jeneshicc/20090908/1252413541
nub' :: (Ord a) => [a] -> [a]
nub' l = nub'' l Set.empty
   where nub'' [] _ = []
         nub'' (x:xs) s
           | x `Set.member` s = nub'' xs s
           | otherwise    = x : nub'' xs (x `Set.insert` s)

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


