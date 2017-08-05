

import Data.Maybe (fromMaybe)
import HasKAL.IOUtils.Function (stdin2vec)
import HasKAL.MonitorUtils.GlitchMon.GlitchMonDAT
import HasKAL.MonitorUtils.GlitchMon.GlitchParam
import HasKAL.MonitorUtils.GlitchMon.PipelineFunction
import HasKAL.DataBaseUtils.FrameFull.Data
import HasKAL.FrameUtils.FrameUtils (getSamplingFrequency)
import HasKAL.Misc.ConfFile (readConfFile)
import qualified HasKAL.PlotUtils.HROOT.PlotGraph3D as H3
import HasKAL.SpectrumUtils.Function (updateMatrixElement,  updateSpectrogramSpec)
import HasKAL.SpectrumUtils.Signature (Spectrum,  Spectrogram)
import Numeric.LinearAlgebra.Data --(saveMatrix, cols, rows)
import System.Environment ( getArgs)


main = do
  (conffile, fsorig', startGPStime') <- getArgs >>= \args -> case (length args) of
    3 -> return (head args, args!!1, args!!2)
    _ -> error "Usage runEventTriggerGenerationSTDIN conffile fs startGPStime STDIN"
  ([sl, ch, sf, tl, wFR, wM, nf, nTS, cTFT, cTFF, cF, cT, minN, nN, maxN, cdfI], [qs])
    <- readConfFile conffile ["segmentLength", "channel", "samplingFrequency",
        "traindatlen", "whnFrequencyResolution", "whnMethod", "nfrequency", "ntimeSlide",
        "cutoffFractionTFT", "cutoffFractionTFF", "cutoffFreq", "clusterThres",
        "minimumClusterNum", "nNeighbor", "maxNtrigg", "cdfInterval"] []
  let fsorig = read fsorig' :: Double
  let startGPStime = read startGPStime' :: Double

  let param = GlitchParam
               { segmentLength = read sl :: Int
--               , channel = "K1:LSC-MICH_CTRL_CAL_OUT_DQ"
               , channel = ch --"H1:LOSC-STRAIN"
               , samplingFrequency = read sf :: Double
             -- * whitening
               , traindatlen = read tl :: Double -- [s] must be > 2 * fs/whnFrequencyResolution
               , whnFrequencyResolution = read wFR :: Double -- [Hz]
               , whtCoeff = []
               , whnMethod = read wM :: WhnMethod
             -- * t-f expression
               , nfrequency = read nf :: Double
               , ntimeSlide = read nTS :: Double
             -- * clustering
               , cutoffFractionTFT = read cTFT :: Double
               , cutoffFractionTFF = read cTFF :: Double
               , cutoffFreq = read cF :: Double
               , clusterThres = read cT :: Double
               , celement = basePixel9
               , minimumClusterNum = read minN :: Int
               , nNeighbor = read nN :: Int
               , maxNtrigg = read maxN :: Int
             -- * clean data finder
               , cdfInterval = read cdfI :: Int
               , cdfparameter = cdfp
               , cgps = Nothing
               , reftime = 0
             -- * for debug
               , debugmode = [DS, DC, TF, CL]
               , debugDir = "production"
               }
      cdfp = CDFParam
              { cdf'samplingFrequency = read sf :: Double
              , cdf'cutoffFrequencyLow = 10
              , cdf'cutoffFrequencyHigh = 500
              , cdf'blockSize = 50
              , cdf'fftSize = nfrequency param
              , cdf'chunkSize = traindatlen param
              }

  v <- stdin2vec
  maybeETG <- runEventTriggerGenerationDAT param (channel param) startGPStime v
  case maybeETG of
    Nothing -> error "Usage runEventTriggerGenerationSTDIN conffile fs startGPStime STDIN"
    Just (etgSpecgram,etgID) -> do
      let (etgT,etgF,etgM) = etgSpecgram
      saveMatrix "clustered_spectrogram.dat" "%lf" etgM
      H3.spectrogramM H3.LogY
                      H3.COLZ
                      "mag"
                      "clustered PixelSNR spectrogram"
                      ("clustered_spectrogram.png")
                      ((0, 0), (0, 0))
                      etgSpecgram
      let retgM = rows etgM
          cetgM = cols etgM
      let zeroMatrix = (retgM >< cetgM) $ replicate (retgM*cetgM) 0.0
      let etgID' = concat etgID
      let tileValues = map (\(x,i)->x) etgID'
      let idValues = map (\(x,i)->fromIntegral i :: Double) etgID'
      let idM = updateSpectrogramSpec etgSpecgram
            $updateMatrixElement zeroMatrix tileValues idValues
      let (idT,idF,idMM) = idM
      saveMatrix "island_ID_map.dat" "%lf" idMM
      H3.spectrogramM H3.LogY
                      H3.COLZ
                      "mag"
                      "island ID map"
                      ("island_ID_map.png")
                      ((0, 0), (0, 0))
                      idM


--"/home/kazu/work/data/gw150914/H-H1_LOSC_4_V1-1126259446-32.gwf"
--  runGlitchMonFile param (channel param) "/data/kagra/raw/full/K-K1_C-1144374208-32.gwf"
