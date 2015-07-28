import HasKAL.FrameUtils.FrameUtils
import NoiseFloorMon
import HasKAL.SimulationUtils.DetectorNoiseGenerator
import qualified HasKAL.DetectorUtils.Detector as HDD 
import qualified Data.Complex as DC

import qualified HasKAL.ExternalUtils.GSL.RandomNumberGeneration as RNG
import qualified HasKAL.ExternalUtils.GSL.RandomNumberDistributions as RND
--import Data.Vector.Storable as VS
import qualified HasKAL.Misc.StrictMapping as HMS

{- For fft -}
import Numeric.GSL.Fourier
import Numeric.LinearAlgebra 
main = do 
   let tsSF = 2048
       lcsz = 32 * tsSF 
       minfreq = 64.0
       maxfreq = 128.0
       printshow xx = putStrLn $ (show (fst xx)) ++ " " ++ (show (snd xx))
   (nfmmean,nfmdev)<- estimateThreshold tsSF minfreq maxfreq lcsz

--   ts <- genTimeGauss lcsz 1.0
   ts <- genTDomain lcsz tsSF 40 1024 HDD.KAGRA
   ts2 <- genTDomain' (10*tsSF) tsSF 40 1024 HDD.KAGRA
   let ts3 = ts ++ ts2 ++ ts
   nfmstatus<-getNoiseFloorStatus ts3 tsSF minfreq maxfreq (nfmmean,nfmdev)
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