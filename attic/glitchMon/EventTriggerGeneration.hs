

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


part'EventTriggerGeneration :: WaveData
                            -> StateT GP.GlitchParam IO (Spectrogram, [[(Tile,ID)]])
part'EventTriggerGeneration wave = do
  param <- get
  (a, s) <- liftIO $ runStateT (section'TimeFrequencyExpression wave) param
  section'Clustering a


section'TimeFrequencyExpression :: WaveData
                                -> StateT GP.GlitchParam IO Spectrogram
section'TimeFrequencyExpression whnWaveData = do
  param <- get
  let refpsd = GP.refpsd param
      fs = GP.samplingFrequency param
      nfreq2 = GP.nfrequency param`div`2
      nfreq = GP.nfrequency param
      ntimeSlide = GP.ntimeSlide param
      ntime = (NL.dim $ gwdata whnWaveData) `div` ntimeSlide
      snrMatF = scale (fs/fromIntegral nfreq) $ fromList [0.0, 1.0..fromIntegral nfreq2]
      snrMatT = scale (fromIntegral nfreq/fs) $ fromList [0.0, 1.0..fromIntegral ntime -1]
      snrMatT' = mapVector (+deformatGPS (startGPSTime whnWaveData)) snrMatT
      snrMatP = (nfreq2><ntime) $ concatMap (\i -> map ((!! i) . (\i->toList $ zipVectorWith (/)
        (
        snd $ gwOnesidedPSDV (subVector (ntimeSlide*i) nfreq (gwdata whnWaveData)) nfreq fs)
        (snd refpsd)
        )) [0..ntime-1]) [0..nfreq2] :: Matrix Double
  return (snrMatT', snrMatF, snrMatP)


section'Clustering :: Spectrogram
                   -> StateT GP.GlitchParam IO (Spectrogram,[[(Tile,ID)]])
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
      survivor = nub $ excludeOnePixelIsland basePixel25 thrsed
      survivorwID = taggingIsland survivor
      excludedIndx = Set.toList $ Set.difference (Set.fromList thrsed) (Set.fromList survivor)
      newM = updateSpectrogramSpec snrMat
       $ updateMatrixElement mg excludedIndx (replicate (length excludedIndx) 0.0)
  return (newM, survivorwID)




