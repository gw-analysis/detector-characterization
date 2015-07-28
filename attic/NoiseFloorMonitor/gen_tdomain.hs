import HasKAL.SimulationUtils.DetectorNoiseGenerator
import qualified HasKAL.ExternalUtils.GSL.RandomNumberGeneration as RNG
import qualified HasKAL.DetectorUtils.Detector as HDD 
import qualified Data.Complex as DC

{- For fft -}
import Numeric.GSL.Fourier
import Numeric.LinearAlgebra

{-- External Functions --}
-- geneNPSD
-- param1: Random Number Generator in GSL
-- param2: Detecotr Type (defined at HasKAL/DetectorUtils/Detector.hs)
-- param3: frequency [Hz] (like as [1.0..1000.0])
-- return: 1st member -> frequency [Hz]
--         2nd member -> h-of-f [/rHz] (real :+ imaginary)

main = do
   let tsSF = 1024.0 ::Double
       ndat = 2048 ::Int
       nfreq = floor (0.5*(fromIntegral ndat))
       ttot = (fromIntegral ndat) / tsSF
       df = 1.0 / ttot
       dt = 1.0 / tsSF
       minfreq = 5
       maxfreq = 1024
       freqlist = 0:(take nfreq [1*df,2*df..])
   rng <-  RNG.newRngWithSeed (0)
   npsd'' <- geneNPSD rng HDD.VIRGO freqlist 
   let npsd' = map (replace_zero minfreq maxfreq) npsd''
       npsdconj = tail.init $ map DC.conjugate $ reverse npsd'
       npsd = npsd' ++ npsdconj
       npsdamp = map DC.magnitude npsd'
       zipnpsd = zip freqlist npsdamp
       printshow xx = putStrLn $ (show (fst xx)) ++ " " ++ (show (snd xx))
       noisetdomain' = ifft (fromList npsd)
       noisetdomain = map realPart $ toList noisetdomain'
       tlist = take ndat [dt,2*dt..]
   mapM_ printshow (zip tlist noisetdomain)



replace_zero :: Double->Double->(Double, DC.Complex Double)->(DC.Complex Double)
replace_zero minfreq maxfreq (freq, npsd) | freq == 0      = (0 DC.:+ 0)
	     	     	     	    	  | freq < minfreq = (0 DC.:+ 0)*npsd
	     	    	  	  	  | freq > maxfreq = (0 DC.:+ 0)*npsd
  					  | otherwise = npsd