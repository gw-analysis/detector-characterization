--import HasKAL.FrameUtils.FrameUtils
import FrameUtils
import HasKAL.FrameUtils.Function
import Data.Maybe(fromJust)
import NoiseFloorMon
import HasKAL.SimulationUtils.DetectorNoiseGenerator
import qualified HasKAL.DetectorUtils.Detector as HDD 
import qualified Data.Complex as DC
import HasKAL.SignalProcessingUtils.Resampling

import qualified HasKAL.ExternalUtils.GSL.RandomNumberGeneration as RNG
import qualified HasKAL.ExternalUtils.GSL.RandomNumberDistributions as RND
--import Data.Vector.Storable as VS
import qualified HasKAL.Misc.StrictMapping as HMS

import HasKAL.SignalProcessingUtils.Resampling



{- For fft -} 
import Numeric.GSL.Fourier  
import Numeric.LinearAlgebra

import HasKAL.SpectrumUtils.SpectrumUtils

import HasKAL.WaveUtils.Data
import Data.Time
import HasKAL.TimeUtils.Function
import HasKAL.TimeUtils.Signature

gpsfirst :: (Int, Int, Double)->(Int, Int)
gpsfirst (t1, t2, _) = (t1, t2) 

gpslast :: Int->(Int, Int)->(Int, Int)
gpslast  t3 (t1, t2)= (t1+t3, t2)

takesig :: (GPSTIME, GPSTIME, Double)->(Double, Double)
takesig (t1, t2, sig) = (deformatGPS t1, sig)

main = do     
   let fname = "/home/yokozawa/L-L1_LOSC_4_V1-931184640-4096.gwf" :: String
       chname = "L1:LOSC-STRAIN" :: String
--   framets' <- readFrameV chname fname 
--   let framets = subVector (4096*16) (4096*64) $ fromJust framets'
   framets' <- genTimeGauss (4096*64)
   let framets = fromList framets'
   let frameSF = 4096.0
--   let np = defaultNFMparam
   np <- makeNFMparam 512.0 300 (1024*10) (1024*5) 512 512 60.0 100.0 6 6
   let psdmedian = gwpsd (toList framets) (whitenFltStride np) (tsreSF np)
   framegps <- getGPSTime fname
   let startGPSTime = gpsfirst $ fromJust framegps
   time1 <- getCurrentTime
   nfmstatus<-getNoiseFloorStatusV framets frameSF startGPSTime np
   time2 <- getCurrentTime
--   print $ diffUTCTime time2 time1
   let printshow xx = putStrLn $ (show (fst xx)) ++ " " ++ (show (snd xx))
--   print (nfmdev np)
--   mapM_ print "fi"
--   mapM_ printshow psdmedian
   mapM_ printshow $ map takesig nfmstatus