--import Numeric.LinearAlgebra  (subVector)
--import Numeric (showFFloat)
--import Control.Monad (forM)
import System.Environment (getArgs)
--import System.Random
--import Data.List.Split (splitOn)

import qualified Data.Complex as DC
import Data.Complex (Complex( (:+) ))

import HasKAL.DetectorUtils.Detector
import HasKAL.SimulationUtils.DetectorNoiseGenerator (geneNPSD)
import HasKAL.SpectrumUtils.DetectorSensitivity (ifonoisepsd)
import qualified HasKAL.ExternalUtils.GSL.RandomNumberGeneration as RNG
import qualified HasKAL.ExternalUtils.GSL.RandomNumberDistributions as RND
import HasKAL.SpectrumUtils.GwPsdMethod
import HasKAL.SpectrumUtils.SpectrumUtils
import UsefulFunction (taple2string, threedata2string)

import qualified Numeric.LinearAlgebra as NLA
import qualified HasKAL.Misc.StrictMapping as HMS
import Numeric.GSL.Fourier (ifft)

{-- need 1 argument = amplitude of seismic noise --}
{-- usage : generateUpconvNoise 1e-6 --}


main = do

 {-- configuration --} 
 [argAmp] <- getArgs
 let amplitude = read (argAmp) ::Double --transiend seismic noise [m]
 print amplitude

 let duration = 2.0::Double
     fs       = 1024.0::Double
     dt       = 1.0/fs::Double
     df       = 1.0/duration::Double
     tau      = 0.1::Double -- attenuation factor of injected seismic noise
     fm      = 15 -- injected seismic frequency [Hz]
     amp_seis = 1e-8::Double -- background 1e-8[m/sqrtHz] @ 1Hz
     gfactor = 5e-20::Double -- typical value at Virgo VSR1
     lambda   = 1064e-6::Double --wavelength of laser[m]
     x0       = 2.0::Double -- arbitrary phase [rad]
     fLow    = 5.0::Double -- low frequency cut off [Hz]
 print duration
 print fs
 print dt

 let tlist = [0, dt..duration-dt]::[Double]
 let flist = [fLow, fLow+df..fs/2]::[Double]
     flistLow = [df..fLow-df]::[Double]
     flistAll = [0.0] ++ flistLow ++ flist ::[Double]

 {-- generate detector noise --}
-- rng <- RNG.newRngWithSeed (-1)
-- flistNoiseTaple<- geneNPSD rng VIRGO flist
-- let flistNoise = map (DC.magnitude.snd) flistNoiseTaple
 let sensCurve = (zip flist).NLA.toList.(ifonoisepsd VIRGO).NLA.fromList $ flist
 psdNoise <- psd2complex sensCurve
 let fNoise = [0:+0] ++ take (length flistLow) [0:+0] ++ psdNoise ++ (map DC.conjugate psdNoise) ++ take (length flistLow) [0:+0]

 let tDetectorNoise' = ifft $ NLA.fromList fNoise
     tDetectorNoise  = map NLA.realPart $ NLA.toList tDetectorNoise'
 let fname = "tnoise1.txt"
 writeFile fname $ taple2string tlist tDetectorNoise


     
 let tDetectorNoise = ifft (NLA.fromList fNoise)
 let psdNoiseAbs = map (DC.magnitude) psdNoise ::[Double]
     psdlow      = take (length flistLow) (repeat (0))
     fpsdNoise   = [(0, 0)] ++  zip flistLow psdlow ++ zip flist psdNoiseAbs
 tDetectorNoise <- psd2ts fpsdNoise
 let fname = "tnoise2.txt"
 writeFile fname $ taple2string tlist tDetectorNoise


 
 {-- convert time domain by IFFT --}



 {-- generate seismic noise --}
 let data_dx = map exponentialSine tlist
             where exponentialSine x = amplitude * sin (2.0 * pi * fm * x) * exp(-x/tau)

 {-- add background of seismic noise --}


 {-- generate upconversion noise --}
 let data_hsc = map upconversionNoise data_dx
              where upconversionNoise x = gfactor * sin (4.0 * pi / lambda * (x + x0))
 let fname = "upconv.txt"
 writeFile fname $ threedata2string tlist data_dx data_hsc

 {-- merge all noise --}
 
  
 {-- output generated noise --}


 return ()


psd2complex :: [(Double,Double)] -> IO [DC.Complex Double]
psd2complex sensCurve = do
--  rng <- RNG.newRngWithSeed (-1)
  rng <- RNG.newRngWithSeed (0)
  HMS.forM' sensCurve $ \(freq, curve) -> do 
    let scurve = sqrt curve
    real <- RND.gslRanGaussian rng (sqrt 0.5)
    imag <- RND.gslRanGaussian rng (sqrt 0.5)
    let getrayleigh = (scurve :+ 0)*(real :+ imag)
    return $ getrayleigh

psd2ts :: [(Double,Double)]->IO [Double]
psd2ts sensCurve = do
  psdcomplex <- psd2complex sensCurve
  let vpsdcomplex = NLA.fromList psdcomplex
      vtscomplex = ifft vpsdcomplex 
  return $ map NLA.realPart $ NLA.toList vtscomplex 
