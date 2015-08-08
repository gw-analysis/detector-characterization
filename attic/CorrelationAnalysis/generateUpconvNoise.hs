import System.Environment (getArgs)
import qualified Data.Complex as DC
import Data.Complex (Complex( (:+) ))

import HasKAL.DetectorUtils.Detector
import HasKAL.SimulationUtils.DetectorNoiseGenerator (geneNPSD)
import HasKAL.SpectrumUtils.DetectorSensitivity (ifonoisepsd)
import qualified HasKAL.ExternalUtils.GSL.RandomNumberGeneration as RNG
import qualified HasKAL.ExternalUtils.GSL.RandomNumberDistributions as RND
import qualified HasKAL.Misc.StrictMapping as HMS
import qualified Numeric.LinearAlgebra as NLA

{-- plan: import HasKAL.SpectrumUtils.FFT --}
import qualified FFT as FFT

{-- plan: module HasKAL.simulationUtils.injectionType( --}
import InjectionType


{-- need 1 argument = amplitude of seismic noise --}
{-- usage : generateUpconvNoise 1e-6 --}

{-- output :  time[sec], delta_x[m], data_h_sc[], GW channel(data_h_sc + noiset), noiset, seismic --}


{-- memo : Norm of FFT is not consistent with C language. --}

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
 rng <- RNG.newRngWithSeed (-1)

 {-- generate detector noise --}
 {-- convert time domain by IFFT --}
 tDetectorNoise <- genDetectorNoise rng VIRGO fLow flist
 let fname = "tnoise.txt"
 writeFile fname $ taple2string tlist tDetectorNoise


 {-- generate seismic noise --}
 let deltax = map (injectExternalForce ExponentialSine amplitude fm tau) tlist
-- let deltax = map (injectExternalForce Sine amplitude fm tau) tlist

 {-- add background of seismic noise --}
 tSeisBG <- genSeismicNoiseT rng fLow ampSeis flist
 let fname = "tnoise2.txt"
 writeFile fname $ taple2string tlist tSeisBG

 let data1 = map (DC.magnitude) $ NLA.toList $ FFT.fft'r2c $ NLA.fromList tSeisBG
 let fname = "fnoise3.txt"
 writeFile fname $ taple2string flist data1

 {-- merge all noise --}
 let tSeismicData = zipWith (+) deltax  tSeisBG
  
 {-- generate upconversion noise --}
 let dataHsc = map upconversionNoise tSeismicData :: [Double]
              where upconversionNoise x = gfactor * sin (4.0 * pi / lambda * (x + x0))

 let tGWchannel = zipWith (+) dataHsc tDetectorNoise

 {-- output generated noise --}
 {-- output :  time[sec], delta_x[m], data_h_sc[], GW channel(data_h_sc + noiset), noiset, seismic --}
 let fname = "upconv.txt"
 writeFile fname $ sixdata2string tlist deltax dataHsc tGWchannel tDetectorNoise tSeismicData
 return () 




{-- Instead of 0 padding, S(f < f_low) = S(f_low) padding --}
replace_zero :: Double -> (Double, DC.Complex Double) -> (DC.Complex Double)
replace_zero minfreq (freq, npsd)
              | 0 <= freq && freq < minfreq = (0 :+ 0)
              | otherwise                     = npsd

{-- HasKAL.simulationUtils.... --}
genDetectorNoise :: RNG.GSLRng -> Detector -> Double -> [Double] -> IO [Double]
genDetectorNoise rng ifo fLow fin = do
   flistNoiseTaple <- geneNPSD rng VIRGO fin
   let fx = map (replace_zero fLow) $ zip fin (map snd flistNoiseTaple) :: [Complex Double]
   return $ NLA.toList $ FFT.ifft'c2r $ NLA.fromList fx

{-- HasKAL.simulationUtils.... --}
genSeismicNoiseT :: RNG.GSLRng -> Double -> Double -> [Double] -> IO [Double]
genSeismicNoiseT rng fLow ampSeis fin = do
   fSeismicNoise <- genSeismicNoiseF rng fLow ampSeis fin
   return $ NLA.toList $ FFT.ifft'c2r $ NLA.fromList fSeismicNoise

{-- HasKAL.simulationUtils.... --}
genSeismicNoiseF :: RNG.GSLRng -> Double -> Double -> [Double] -> IO [DC.Complex Double]
genSeismicNoiseF rng fLow ampSeis fin = do
   let seedSf = map (powSeismicNoise fLow ampSeis) fin
   HMS.forM' seedSf $ \sf -> do 
      real <- RND.gslRanGaussian rng (sqrt 0.5)
      imag <- RND.gslRanGaussian rng (sqrt 0.5)
      return $ (sf :+ 0) * (real :+ imag)

{-- internal function --}
{-- HasKAL.simulationUtils.... --}
powSeismicNoise :: Double -> Double -> Double -> Double
powSeismicNoise fLow ampSeis f 
                       | f < fLow  = 0.0
                       | otherwise = ampSeis/f/f

{-- HasKAL.simulationUtils.... --}
injectExternalForce :: InjectionType -> Double -> Double -> Double -> Double -> Double
injectExternalForce injeType amplitude fm tau x = 
                    case injeType of 
                         ExponentialSine -> amplitude * sin (2.0 * pi * fm * x) * exp(-x/tau)
                         Sine -> amplitude * sin (2.0 * pi * fm * x)

{-- to output data --}
taple2string ::[Double] -> [Double] -> String
taple2string a b = unlines  $ zipWith (++) (map show a)  $ zipWith (++) (repeat " ")  (map show b)

{-- internal function of this source --}
threedata2string ::[Double] -> [Double] ->[Double]-> String
threedata2string a b c = unlines  $ zipWith (++) (map show a)  $ zipWith (++) (repeat " ")  $ zipWith (++) ( map show b) $ zipWith (++) (repeat " ")  (map show c)

{-- internal function of this source --}
sixdata2string::[Double] -> [Double] ->[Double]->[Double]->[Double]->[Double]-> String
sixdata2string a b c d e f = unlines  $ zipWith (++) (map show a)  $ zipWith (++) (repeat " ")  $ zipWith (++) ( map show b) $ zipWith (++) (repeat " ")  $ zipWith (++) ( map show c) $ zipWith (++) (repeat " ")  $ zipWith (++) ( map show d) $ zipWith (++) (repeat " ")  $ zipWith (++) ( map show e) $ zipWith (++) (repeat " ")  ( map show f)

