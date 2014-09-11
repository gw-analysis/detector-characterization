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
      (whnNum, (whnDenom, rho)) = lpefCoeff nC $ map ((fromIntegral datLen)*) trainpsdInterp


  -- gwdat on Fourier domain
  let gwdat1 = take datLen $ drop trainDatLen gwdat
      fftGwdat1 = toList.fft.toComplex $ (windowed (hanning datLen) (fromList gwdat1), constant 0 datLen)
      whnCoeff = map magnitude (toList.fft.fromList $ [x:+0|x<-(map (/(sqrt rho)) whnDenom) ++ (replicate (datLen-nC) 0)])
      whnGwfft = zipWith (\x y-> x:+y) (zipWith (*) whnCoeff [x|x:+_<-fftGwdat1]) [x|_:+x<-fftGwdat1]
      whnGwdat = map realPart $ toList.ifft.fromList $ whnGwfft






  -- for confirmation
--  print samplingFrequency
  let tV = [y/16384 |y<-[0.0..(realToFrac datLen) -1.0]] :: [Double]
  HR.plot HR.Linear HR.Line ("time",  "amplitude") "after" "plotHofT.png" $ zip tV whnGwdat
  HR.plot HR.LogXY HR.Line ("frequency [Hz]",  "asd[Hz^-1/2]") "after" "plotTrainPsd.png" $ zip trainfV whnCoeff
  HR.plot HR.LogXY HR.Line ("frequency [Hz]",  "asd[Hz^-1/2]") "after" "plotWhitenedPsd.png" $ gwpsd whnGwdat datLen samplingFrequency

