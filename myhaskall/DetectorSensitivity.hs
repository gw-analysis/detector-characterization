module DetectorSensitivity
       (ifonoisepsd
       , aligoPsd
       , kagraPsd
       , advirgoPsd
       ) where
import Numeric.LinearAlgebra
import Detector

ifonoisepsd :: Detector -> Vector Double -> Vector Double
ifonoisepsd ifo fin = case ifo of
  LIGO  -> aligoPsd fin
  KAGRA -> kagraPsd fin
  VIRGO -> advirgoPsd fin


aligoPsd :: Vector Double -> Vector Double
aligoPsd fin = mapVector psdmodel x
  where
    f0 = 215 :: Double
    psd_scale = 1.0E-49 :: Double
    x  = mapVector (/f0) fin
    psdmodel y = psd_scale * (y**(-4.14) - 5*y**(-2) + 111*(1-y**2+y**4/2)/(1+y**2/2))


kagraPsd :: Vector Double -> Vector Double
kagraPsd fin = mapVector psdmodel x
  where
    f0 = 100 :: Double
    x  = mapVector toLog fin
    toLog z = log z/f0
    psdmodel y = (6.499*1.0E-25*(9.72*10e-9*(exp (-1.43-9.88*y-0.23*y**2))+1.17*(exp (0.14-3.10*y-0.26*y**2))+1.70*(exp (0.14+1.09*y-0.013*y**2))+1.25*(exp (0.071+2.83*y-4.91*y**2))))**2


advirgoPsd :: Vector Double -> Vector Double
advirgoPsd fin = mapVector psdmodel x
  where
    f0 = 300 :: Double
    x  = mapVector toLog fin
    toLog z = log z/f0
    psdmodel y = (1.259*1.0E-24*(0.07*(exp (-0.142-1.437*y+0.407*y**2))+3.10*(exp (-0.466-1.043*y-0.548*y**2))+0.40*(exp (-0.304+2.896*y-0.293*y**2))+0.09*(exp (1.466+3.722*y-0.984*y**2))))**2


