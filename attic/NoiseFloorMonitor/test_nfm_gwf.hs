import HasKAL.FrameUtils.FrameUtils
--import HasKAL.FrameUtils.Function (readFrameWaveData)
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
 

main = do    
--   ts <- genTimeGauss (4096*128) 1.0
--   ts1 <- genTDomain lcsz tsSF 40 1024 HDD.KAGRA
--   ts2 <- genTDomain' (10*tsSF) tsSF 40 1024 HDD.KAGRA
--   let ts = ts1 ++ ts2 ++ ts1
   let fname = "/home/yokozawa/L-L1_LOSC_4_V1-971558912-4096.gwf" :: String
       chname = "L1:LOSC-STRAIN" :: String
       gpstime = 971558912 :: Integer
       obstime = 128 :: Integer
   framets <- readFrame chname fname 
   print "aa"
--   framets <- readFrameWaveData gpstime obstime chname fname
--   frameSF <- getSamplingFrequency fname chname
   let frameSF = 4096.0

   let np = defaultNFMparam
       lcsz = 4096 * 128 
       ts = take lcsz $ map realToFrac (eval framets)
       printshow xx = putStrLn $ (show (fst xx)) ++ " " ++ (show (snd xx))
       nfmmean = 5.481560468892423e-2
       nfmdev = 1.5462956889872793e-2
--   (nfmmean,nfmdev)<- estimateThreshold np (lcsz*2)
--   print "aa"
   nfmstatus<-getNoiseFloorStatus ts frameSF np (nfmmean,nfmdev)
   mapM_ printshow nfmstatus


genTimeGauss :: Int->Double->IO[Double]
genTimeGauss lcsz sigma = do
   let simnoise = Prelude.take lcsz [1,1..] ::[Double]
   rng <- RNG.newRngWithSeed (-1)
   HMS.forM' simnoise $ \j -> do
     gaus <- RND.gslRanGaussian rng sigma
     return $ gaus     

genTDomain :: Int->Int->Double->Double->HDD.Detector->IO[Double]
genTDomain ndat tsSF minfreq maxfreq ifo = do
   let nfreq = floor (0.5*(fromIntegral ndat))
       ttot = (fromIntegral ndat) / (fromIntegral tsSF)
       df = 1.0 / ttot
       dt = 1.0 / (fromIntegral tsSF)
       freqlist = 0:(take nfreq [1*df,2*df..])
   rng <-  RNG.newRngWithSeed (-1)
   npsd'' <- geneNPSD rng ifo freqlist 
   let npsd' = map (replace_zero minfreq maxfreq) npsd''
       npsdconj = tail.init $ map DC.conjugate $ reverse npsd'
       npsd = npsd' ++ npsdconj
       npsdamp = map DC.magnitude npsd'
       zipnpsd = zip freqlist npsdamp
       printshow xx = putStrLn $ (show (fst xx)) ++ " " ++ (show (snd xx))
       noisetdomain' = ifft (fromList npsd)
       noisetdomain = map realPart $ toList noisetdomain'
       tlist = take ndat [dt,2*dt..]
   return $ noisetdomain

genTDomain' :: Int->Int->Double->Double->HDD.Detector->IO[Double]
genTDomain' ndat tsSF minfreq maxfreq ifo = do
   let nfreq = floor (0.5*(fromIntegral ndat))
       ttot = (fromIntegral ndat) / (fromIntegral tsSF)
       df = 1.0 / ttot
       dt = 1.0 / (fromIntegral tsSF)
       freqlist = 0:(take nfreq [1*df,2*df..])
   rng <-  RNG.newRngWithSeed (-1)
   npsd'' <- geneNPSD rng ifo freqlist 
   let npsdd' = map (replace_zero minfreq maxfreq) npsd''
       npsd' = map (*(2 DC.:+ 0)) npsdd' 
       npsdconj = tail.init $ map DC.conjugate $ reverse npsd'
       npsd = npsd' ++ npsdconj
       npsdamp = map DC.magnitude npsd'
       zipnpsd = zip freqlist npsdamp
       printshow xx = putStrLn $ (show (fst xx)) ++ " " ++ (show (snd xx))
       noisetdomain' = ifft (fromList npsd)
       noisetdomain = map realPart $ toList noisetdomain'
       tlist = take ndat [dt,2*dt..]
   return $ noisetdomain



replace_zero :: Double->Double->(Double, DC.Complex Double)->(DC.Complex Double)
replace_zero minfreq maxfreq (freq, npsd) | freq == 0      = (0 DC.:+ 0)
	     	     	     	    	  | freq < minfreq = (0 DC.:+ 0)*npsd
	     	    	  	  	  | freq > maxfreq = (0 DC.:+ 0)*npsd
  					  | otherwise = npsd