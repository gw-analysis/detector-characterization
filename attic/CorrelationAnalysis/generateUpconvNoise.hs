
import System.Environment (getArgs)
import qualified Data.Complex as DC
import Data.Complex (Complex( (:+) ))

import HasKAL.DetectorUtils.Detector
import HasKAL.SimulationUtils.DetectorNoiseGenerator (geneNPSD)
import HasKAL.SpectrumUtils.DetectorSensitivity (ifonoisepsd)
import qualified HasKAL.ExternalUtils.GSL.RandomNumberGeneration as RNG
import qualified HasKAL.ExternalUtils.GSL.RandomNumberDistributions as RND
import UsefulFunction (taple2string, threedata2string)
import qualified HasKAL.Misc.StrictMapping as HMS
import qualified Numeric.LinearAlgebra as NLA
import Numeric.GSL.Fourier (ifft)

--import qualified FFT as FFT

{-- need 1 argument = amplitude of seismic noise --}
{-- usage : generateUpconvNoise 1e-6 --}


main = do

 {-- configuration --} 
 [argAmp] <- getArgs
 let amplitude = read (argAmp) ::Double --transiend seismic noise [m]
 print amplitude

 let duration = 1.0::Double
     fs       = 1024.0::Double
     dt       = 1.0/fs::Double
     df       = 1.0/duration::Double
     tau      = 0.1::Double -- attenuation factor of injected seismic noise
     fm      = 15 -- injected seismic frequency [Hz]
     ampSeis = 1e-8::Double -- background 1e-8[m/sqrtHz] @ 1Hz
     gfactor = 5e-20::Double -- typical value at Virgo VSR1
     lambda   = 1064e-6::Double --wavelength of laser[m]
     x0       = 2.0::Double -- arbitrary phase [rad]
     fLow    = 5.0::Double -- low frequency cut off [Hz]

 let tlist = [0, dt..duration-dt]::[Double]
 let flist    = [0,df..fs/2]::[Double]
     flistRev = [1*df, 2*df..fs/2.0-1*df]::[Double]
     flistAll = flist ++ reverse flistRev :: [Double]

 {-- generate detector noise --}
 rng <- RNG.newRngWithSeed (0)
 -- geneNPSD :: RNG.GSLRng -> HDD.Detector -> [Double] -> IO [(Double, DC.Complex Double)]
 flistNoiseTaple <- geneNPSD rng VIRGO flist
 let flistNoiseTapleRev = map DC.conjugate.reverse.init.tail $ map snd (flistNoiseTaple) :: [DC.Complex Double]
     fNoise'  = map snd flistNoiseTaple ++ flistNoiseTapleRev ::[DC.Complex Double]
     fNoise   = map (replace_zero fLow) $ zip flistAll fNoise'
 let fabs' = map (replace_zero fLow) $ zip flist (map snd flistNoiseTaple)
     fabs  = map (DC.magnitude) fabs'
 let fname = "fnoise1.txt"
 writeFile fname $ taple2string flist fabs


 {-- convert time domain by IFFT --}
 let  tDetectorNoise' = ifft $ NLA.fromList fNoise
      tDetectorNoise  = map NLA.realPart $ NLA.toList tDetectorNoise'
 let fname = "tnoise1.txt"
 writeFile fname $ taple2string tlist tDetectorNoise


 {-- check --}
 -- let fx = map (replace_zero fLow) $ zip flist (map snd flistNoiseTaple)
 -- let tDetectorNoise2 = FFT.ifft'c2r $ NLA.fromList fx
 -- let fname = "tnoise2.txt"
 -- writeFile fname $ taple2string tlist $ NLA.toList tDetectorNoise2



 {-- generate seismic noise --}
 let data_dx = map exponentialSine tlist
             where exponentialSine x = amplitude * sin (2.0 * pi * fm * x) * exp(-x/tau)


 {-- add background of seismic noise --}
 let fSeismic' = map (geneSeismicNoise fLow) flist
               where geneSeismicNoise fLow f
                                      | f < fLow  = 0.0
                                      | otherwise = ampSeis/f/f
-- let fSeismic  = 
 let fname = "fnoise2.txt"
 writeFile fname $ taple2string flist fSeismic'

 


 {-- generate upconversion noise --}
 let data_hsc = map upconversionNoise data_dx
              where upconversionNoise x = gfactor * sin (4.0 * pi / lambda * (x + x0))
 let fname = "upconv.txt"
 writeFile fname $ threedata2string tlist data_dx data_hsc

 {-- merge all noise --}
 
  
 {-- output generated noise --}
 

 return ()

replace_zero :: Double->(Double, DC.Complex Double)->(DC.Complex Double)
replace_zero minfreq (freq, npsd)
              | 0 <= freq && freq < minfreq = (0 :+ 0)
              | otherwise                     = npsd




-- geneSeismicNoise :: RNG.GSLRng -> [Double] -> [Double] -> IO [(ouble, DC.Complex Double)]
-- geneSeismicNoise 

-- fin seedSpectrum = do
--   let sensCurve = (zip fin).(map sqrt).NLA.toList.(HSD.ifonoisepsd ifo).NLA.fromList $ fin
--   HMS.forM' sensCurve $ \(freq, curve) -> do 
--     real <- RND.gslRanGaussian rng (sqrt 0.5)
--     imag <- RND.gslRanGaussian rng (sqrt 0.5)
--     return $ (freq, (curve :+ 0) * (real :+ imag) )eneNPSD :: RNG.GSLRng -> HDD.Detector -> [Double] -> IO [(Double, DC.Complex Double)]
