{-
 - unit test of whitening filter based on linear prediction error filter on
 - Fourier domain
 - for compilation
 - ghc --make testLPEF.hs -lFrame
 -}


import HasKAL.FrameUtils.FrameUtils
import HasKAL.SpectrumUtils.SpectrumUtils
import qualified HasKAL.PlotUtils.HROOT.PlotGraph as HR
import Data.Complex
import Numeric.LinearAlgebra
import Numeric.GSL.Fourier
import HasKAL.SignalProcessingUtils.WindowType
import HasKAL.SignalProcessingUtils.WindowFunction
--import HasKAL.SignalProcessingUtils.ButterWorth
--import HasKAL.SignalProcessingUtils.FilterType
import HasKAL.SignalProcessingUtils.LinearPrediction
import HasKAL.SignalProcessingUtils.Interpolation
import HasKAL.SignalProcessingUtils.InterpolationType

main = do
  let nC = 1500
  let fname = "H-H2_RDS_C03_L2-877201786-128.gwf"
  ch <- getChannelList fname
  let [(chname,  _)] = ch
  samplingFrequency <- getSamplingFrequency fname chname
  gwdat' <- readFrame chname fname
  let gwdat =  map realToFrac (eval gwdat')
      lengwdat = length gwdat

  let datLen = truncate samplingFrequency*2
      trainDatLen = truncate $ samplingFrequency*10

  -- estimating spectrum density
  let trainDat = take trainDatLen gwdat
      trainNfft= truncate samplingFrequency
      (trainfV, trainpsd) = unzip $ gwpsd trainDat trainNfft samplingFrequency
      trainpsdInterp = interpV trainfV trainpsd (toList $ linspace datLen (0, samplingFrequency)) Linear
      whnCoeff = lpefCoeff (nC-1) $ map ((fromIntegral datLen)*) trainpsdInterp

  -- gwdat on Fourier domain
  let gwdat1 = take datLen $ drop trainDatLen gwdat
      fftGwdat1 = fft.toComplex $ (windowed (hanning datLen) (fromList gwdat1), constant 0 datLen)
      fft_whnCoeff = mapVector magnitude $ fft $ join [fromList [x:+0|x<-whnCoeff],constant (0:+0) (datLen-nC)]
      whnGwfft = toComplex (fft_whnCoeff*(mapVector realPart fftGwdat1), mapVector imagPart fftGwdat1)
      whnGwdat = mapVector realPart (ifft whnGwfft)






  -- for confirmation
--  print samplingFrequency
  let tV = [y/16384 |y<-[0.0..(realToFrac datLen) -1.0]] :: [Double]
  HR.plot HR.Linear HR.Line ("time",  "amplitude") "after" "plotHofT.png" $ zip tV (toList whnGwdat)
  HR.plot HR.LogXY HR.Line ("frequency [Hz]",  "asd[Hz^-1/2]") "after" "plotTrainPsd.png" $ zip trainfV (toList fft_whnCoeff)
  HR.plot HR.LogXY HR.Line ("frequency [Hz]",  "asd[Hz^-1/2]") "after" "plotWhitenedPsd.png" $ gwpsd (toList whnGwdat) datLen samplingFrequency

