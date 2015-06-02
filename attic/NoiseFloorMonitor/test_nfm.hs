import HasKAL.FrameUtils.FrameUtils
import HasKAL.MonitorUtils.NoiseFloorMon.NoiseFloorMon

import qualified HasKAL.ExternalUtils.GSL.RandomNumberGeneration as RNG
import qualified HasKAL.ExternalUtils.GSL.RandomNumberDistributions as RND
--import Data.Vector.Storable as VS
import qualified HasKAL.Misc.StrictMapping as HMS

main = do 
   let tsSF = 2048
       lcsz = 32 * tsSF
       minfreq = 64.0
       maxfreq = 128.0
       printshow xx = putStrLn $ (show (fst xx)) ++ " " ++ (show (snd xx))
   (nfmmean,nfmdev)<- estimateThreshold tsSF minfreq maxfreq lcsz

   ts <- genTimeGauss lcsz
--   let frame = "K-K1_R-1113204031-32.gwf" ::String
--       chname = "ADC3"
--   fdata <- readFrame chname frame
--   print "OK"
--   let ts =  map realToFrac (eval fdata)  
   nfmstatus<-getNoiseFloorStatus ts tsSF minfreq maxfreq (nfmmean,nfmdev)
   mapM_ printshow nfmstatus
--   mapM_ print simnoise

genTimeGauss :: Int->IO[Double]
genTimeGauss lcsz = do
   let simnoise = Prelude.take lcsz [1,1..] ::[Double]
   rng <- RNG.newRngWithSeed (-1)
   HMS.forM' simnoise $ \j -> do
     gaus <- RND.gslRanGaussian rng 1.0 
     return $ gaus     