

module GlitchMon.EventTriggerGeneration
( part'EventTriggerGeneration
) where



import Control.Monad.State (StateT, runStateT, execStateT, get, put, liftIO)
import Data.List (nub,  foldl',  elemIndices,  maximum,  minimum,  lookup)
import qualified Data.Set as Set
import HasKAL.MathUtils.FFTW (dct2d, idct2d)
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
  section'Clustering a


section'TimeFrequencyExpression :: WaveData
                                -> StateT GP.GlitchParam IO Spectrogram
section'TimeFrequencyExpression whnWaveData = do
  liftIO $ print "start time-frequency expansion"
  param <- get
  let refpsd = GP.refpsd param
      fs = GP.samplingFrequency param
      nfreq2 = GP.nfrequency param`div`2
      nfreq = GP.nfrequency param
      ntimeSlide = GP.ntimeSlide param
      ntime = ((NL.dim $ gwdata whnWaveData)-nfreq) `div` ntimeSlide
      snrMatF = scale (fs/fromIntegral nfreq) $ fromList [0.0, 1.0..fromIntegral nfreq2-1]
      snrMatT = scale (fromIntegral nfreq/fs) $ fromList [0.0, 1.0..fromIntegral ntime -1]
      snrMatT' = mapVector (+deformatGPS (startGPSTime whnWaveData)) snrMatT
      snrMatP = flipud . NL.takeRows nfreq2 $ NL.fromColumns (map calcSpec [0..ntime-1]) :: Matrix Double
        where calcSpec tindx = NL.zipVectorWith (/)
                (snd $ gwOnesidedPSDV (NL.subVector (ntimeSlide*tindx) nfreq (gwdata whnWaveData)) nfreq fs)
                (snd refpsd)
      out = (snrMatT', snrMatF, snrMatP)
  liftIO $ H3.spectrogramM H3.LogY
                          H3.COLZ
                          "mag"
                          "pixelSNR spectrogram"
                          "gw150914_pixelSNR_spectrogram.eps"
                          ((0, 0), (0, 0))
                          out
  liftIO $ H.plot H.Linear
                  H.Line
                  1
                  H.RED
                  ("time","amplitude")
                  0.05
                  "whitened data"
                  "gw150219_whitened_timeseries.eps"
                  ((0,0),(0,0))
                  $ zip [0,1/4096..] (NL.toList $ gwdata whnWaveData)
  return out


section'Clustering :: Spectrogram
                   -> StateT GP.GlitchParam IO (Spectrogram,[[(Tile,ID)]])
section'Clustering (snrMatT, snrMatF, snrMatP') = do
  liftIO $ print "start seedless clustering"
  param <- get
  let dcted' = dct2d snrMatP'
      ncol = cols dcted'
      nrow = rows dcted'
      zeroElementc = [(x, y) | x<-[0..nrow-1], y<-[ncol-GP.resolvTime param..ncol-1]]
      zeroElementr = [(x, y) | y<-[0..ncol-1], x<-[nrow-GP.resolvFreq param..nrow-1]]
      zeroElement = zeroElementr ++ zeroElementc
      dcted = updateMatrixElement dcted' zeroElement $ take (length zeroElement) [0, 0..]
      snrMatP = idct2d dcted
  let thresIndex = head $ NL.find (>=GP.cutoffFreq param) snrMatF
      snrMat = (snrMatT, NL.subVector thresIndex (nrow-thresIndex) snrMatF, NL.dropRows thresIndex snrMatP)
      (_, _, mg) = snrMat
      thrsed = NL.find (>=GP.clusterThres param) mg
      survivor = thrsed -- nub' $ excludeOnePixelIsland basePixel25 thrsed
  liftIO $ print $ length survivor
  let survivorwID = taggingIsland survivor
      excludedIndx = Set.toList $ Set.difference (Set.fromList thrsed) (Set.fromList survivor)
      newM = updateSpectrogramSpec snrMat
       $ updateMatrixElement mg excludedIndx (replicate (length excludedIndx) 0.0)
  liftIO $ print "finishing ETG section.."
  return (newM, survivorwID)



-- | O (nlog n) nub 
-- | lent from http://d.hatena.ne.jp/jeneshicc/20090908/1252413541
nub' :: (Ord a) => [a] -> [a]
nub' l = nub'' l Set.empty
   where nub'' [] _ = []
         nub'' (x:xs) s
           | x `Set.member` s = nub'' xs s
           | otherwise    = x : nub'' xs (x `Set.insert` s)


