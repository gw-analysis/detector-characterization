import HasKAL.FrameUtils.FrameUtils
import SpectrumUtilsNoiseMon
import LinearPredictionNoiseMon
import HasKAL.SignalProcessingUtils.Resampling

{- For plotting -}
--import HasKAL.PlotUtils.PlotOption.PlotOptionHROOT
--import qualified HasKAL.PlotUtils.HROOT.PlotGraph as HR

{- For fft -}
import Numeric.GSL.Fourier
import Numeric.LinearAlgebra

import HROOT hiding (eval)

main = do
     let fname = "/home/yokozawa/L-L1_RDS_C03_L2-877264406-128.gwf" ::String
     chList <- getChannelList fname
     let chname = head $ map fst chList
     fdata <- readFrame chname fname
     --sf <- getSamplingFrequency "/home/yokozawa/L-L1_RDS_C03_L2-877264406-128.gwf" "L1:LSC-STRAIN"	
     let sf = 16384 :: Double
         dat   = map realToFrac (eval fdata)         
         nfft = 4096 :: Int   
	 downsf = 4096 :: Double
	 maxf = 2048 ::Int
	 downdat = downsample sf downsf dat
         psdmedian  =  gwpsd dat nfft sf :: [(Double, Double)]
--         psdmedian  =  gwpsd dat nfft sf :: [(Double, Double)]
	 psdoneshot =  gwpsdOneshot downdat nfft downsf :: [(Double, Double)]
	 datw  = whitening (lpefCoeff 1000 psdmedian) dat  
	 psdwmedian = gwpsd datw nfft sf :: [(Double, Double)]
	 psdwoneshot = gwpsdOneshot datw nfft sf :: [(Double, Double)]
         printshow xx = putStrLn $ (show (fst xx)) ++ " " ++ (show (snd xx)) 
	 dat2 = map (\x -> x*x) datw
	 dat2w = map (\x -> x*x) datw
	 nrunmed = 16384 :: Int
	 datrunmed = runmed dat2w nrunmed
	 intervalrunmed = (fromIntegral nrunmed) / sf
	 trunmed = [(intervalrunmed),(intervalrunmed*2)..]
	 datrunmedtuple = zip trunmed datrunmed
	 datzip = zip [1..] datw
     mapM_ printshow $ take maxf datrunmedtuple 
--     mapM_ printshow psdwwoneshot    
--     mapM_ printshow datzip 

--     HR.plot HR.LogXY HR.Line ("frequency","Spectrum") "PowerSpectrum" "PSDPlot.eps" $ psdmedian



