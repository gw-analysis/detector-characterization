import HasKAL.FrameUtils.FrameUtils
import SpectrumUtilsNoiseMon
import LinearPredictionNoiseMon

{- For plotting -}
--import HasKAL.PlotUtils.PlotOption.PlotOptionHROOT
--import qualified HasKAL.PlotUtils.HROOT.PlotGraph as HR

{- For fft -}
import Numeric.GSL.Fourier
import Numeric.LinearAlgebra

import HROOT hiding (eval)

main = do
     fdata <- readFrame "L1:LSC-STRAIN" "/home/yokozawa/L-L1_RDS_C03_L2-877264406-128.gwf"
     let dat   = map realToFrac (eval fdata)
         sf = 16384 :: Double 
         nfft = 16384 :: Int   
         psdmedian  =  gwpsd dat nfft sf :: [(Double, Double)]
	 psdoneshot =  gwpsdOneshot dat nfft sf :: [(Double, Double)]
	 datw  = whitening (lpefCoeff 1000 psdmedian) dat  
	 psdwmedian = gwpsd datw nfft sf :: [(Double, Double)]
	 psdwoneshot = gwpsdOneshot datw nfft sf :: [(Double, Double)]
         printshow xx = putStrLn $ (show (fst xx)) ++ " " ++ (show (snd xx)) 
	 dat2 = map (^2) datw
	 dat2w = map (^2) datw
	 nrunmed = 16384 :: Int
	 datrunmed = runmed dat2w nrunmed
	 intervalrunmed = (fromIntegral nrunmed) / sf
	 trunmed = [(intervalrunmed),(intervalrunmed*2)..]
	 datrunmedtuple = zip trunmed datrunmed
     mapM_ printshow datrunmedtuple
--     mapM_ printshow psdwwoneshot    


--     HR.plot HR.LogXY HR.Line ("frequency","Spectrum") "PowerSpectrum" "PSDPlot.eps" $ psdmedian



