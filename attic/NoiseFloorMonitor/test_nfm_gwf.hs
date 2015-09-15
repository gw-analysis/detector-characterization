import HasKAL.FrameUtils.FrameUtils
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

import HasKAL.WaveUtils.Data
import Data.Time
import HasKAL.TimeUtils.Function

gpsfirst :: (Int, Int, Double)->(Int, Int)
gpsfirst (t1, t2, _) = (t1, t2) 

gpslast :: Int->(Int, Int)->(Int, Int)
gpslast  t3 (t1, t2)= (t1+t3, t2)

main = do     
   let fname = "/home/yokozawa/L-L1_LOSC_4_V1-931184640-4096.gwf" :: String
       chname = "L1:LOSC-STRAIN" :: String
   framets' <- readFrameV chname fname 
--   let framets = subVector 0 (4096*128) $ fromJust framets'
   let framets = fromJust framets'
--   print framets
--   frameSF' <- getSamplingFrequency fname chname
--   print frameSF'
--   let frameSF = fromJust frameSF'
--   print frameSF'
   let frameSF = 4096.0
   let np = defaultNFMparam
   framegps <- getGPSTime fname
   let startGPSTime = gpsfirst $ fromJust framegps
   time1 <- getCurrentTime
   nfmstatus<-getNoiseFloorStatusV framets frameSF startGPSTime np
   time2 <- getCurrentTime
   print $ diffUTCTime time2 time1
   mapM_ print nfmstatus
