module SpiralorRingdownSnr
       (snrInspiral
       )where

import DetectorSensitivity
import Detector
import Numeric.LinearAlgebra

integratedInspiral :: Vector Double -> Vector Double -> Vector Double -> Detector -> Vector Double -> Vector Double
integratedInspiral m1 m2 d ifo fin = (const*(integmass**(5/6)) *((5*symmmass/6)**(1/2))/(d*pi**(2/3)))**(2) * fin**(-7/3)/(ifonoisepsd ifo fin) 
  where integmass = m1 + m2
        symmmass = m1*m2/((m1 + m2)**2)
        const = c*(g/c**3) * (5/6)
          where c = 2.99*10**8
                g = 6.67*10**(-11)

snrInspiralPow2 :: Vector Double ->  Vector Double ->  Vector Double -> Detector ->  Vector Double -> Vector Double -> Vector Double
snrInspiralPow2 m1 m2 d ifo flower df
    | m1 <  0 =  error "mass 1: Why did you insert a minus number?"
    | m2 <  0 =  error "mass 2 : Why did you insert a minus number?"
    | d <  0 =  error "distance : Why did you insert a minus number?"
    | flower <  0 =  error "lower frequency : Why did you insert a minus number?"
    | df <  0 =  error "df : Why did you insert a minus number?"
    | flower > fupp =  0
    | otherwise = (integratedInspiral m1 m2 d ifo flower)*df + (snrInspiralPow2 m1 m2 d ifo (flower +  df)  df)
  where fupp = 1/((6**(3/2)) *pi*(m1 + m2)*(g/(c**3)))
          where c = 2.99*10**8
                g = 6.67*10**(-11)


snrInspiral :: Vector Double ->  Vector Double ->  Vector Double -> Detector ->  Vector Double -> Vector Double -> Vector Double
snrInspiral m1 m2 d ifo flower df = (snrInspiralPow2 m1 m2 d ifo flower df)**(1/2)
