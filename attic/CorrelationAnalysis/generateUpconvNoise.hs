--import Numeric.LinearAlgebra  (subVector)
--import Numeric (showFFloat)
--import Control.Monad (forM)
import System.Environment (getArgs)
--import System.Random
--import Data.List.Split (splitOn)

import HasKAL.SpectrumUtils.GwPsdMethod
import HasKAL.SpectrumUtils.SpectrumUtils
import UsefulFunction (taple2string, threedata2string)

{-- need 1 argument = amplitude of seismic noise --}
{-- usage : generateUpconvNoise 1e-6 --}


main = do

 {-- configuration --} 
 [arg_amp] <- getArgs
 let amplitude = read (arg_amp) ::Double --transiend seismic noise [m]
 print amplitude

 let duration = 1.0::Double
     fs       = 1024.0::Double
     dt       = 1.0/fs::Double
     npoint   = floor $ (duration * fs)::Int
     tau      = 0.1::Double -- attenuation factor of injected seismic noise
     f_m      = 15 -- injected seismic frequency [Hz]
     amp_seis = 1e-8::Double -- background 1e-8[m/sqrtHz] @ 1Hz
     g_factor = 5e-20::Double -- typical value at Virgo VSR1
     lambda   = 1064e-6::Double --wavelength of laser[m]
     x0       = 2.0::Double -- arbitrary phase [rad]
 print duration
 print fs
 print dt
 print npoint


 {-- generate detector noise --}
 let tlist   = [0, dt..duration-dt]::[Double]


 {-- convert time domain by IFFT --}


 {-- generate seismic noise --}
 let data_dx = map exponentialSine tlist
             where exponentialSine x = amplitude * sin (2.0 * pi * f_m * x) * exp(-x/tau)

 {-- add background of seismic noise --}


 {-- generate upconversion noise --}
 let data_hsc = map upconversionNoise data_dx
              where upconversionNoise x = g_factor * sin (4.0 * pi / lambda * (x + x0))
 let fname = "hoge.txt"
 writeFile fname $ threedata2string tlist data_dx data_hsc

 {-- merge all noise --}
 
  
 {-- output generated noise --}


 return ()
