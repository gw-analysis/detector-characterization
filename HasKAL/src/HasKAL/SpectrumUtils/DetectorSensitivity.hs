module HasKAL.SpectrumUtils.DetectorSensitivity
  (ifonoisepsd
  ) where


-- analytical expression of the design noise curves of aLIGO, adVirgo, KAGRA
-- aLIGO:
-- on a table in page 42 in the paper http://arxiv.org/abs/0903.0338
-- adVirgo:
-- eq. 6 in page 5 in the paper http://arxiv.org/abs/1202.4031
-- KAGRA:
-- eq. 5 in paper 4 in the paper http://arxiv.org/abs/1202.4031
-- ET:
-- eq. 2.2 and 2.3 in page 5 in the paper http://arxiv.org/abs/1005.0304
-- eLISA:
-- eq. 1 in page 3 in the page http://arxiv.org/abs/1005.0304
-- DECIGO:
-- eq. 1 in page 2 in the paper http://arxiv.org/abs/1110.2865
--
--
-- add LIGO_Livingston, Hanford to ifonoisepsd by Yokozawa 2014/07/06

import Numeric.LinearAlgebra
import HasKAL.DetectorUtils.Detector

ifonoisepsd :: Detector -> Vector Double -> Vector Double
ifonoisepsd ifo fin = case ifo of
  LIGO            -> aligoPsd fin
  LIGO_Livingston -> aligoPsd fin
  LIGO_Hanford    -> aligoPsd fin
  KAGRA           -> kagraPsd fin
  VIRGO           -> advirgoPsd fin
  ET              -> etPsd fin
  ELISA           -> elisaPsd fin
  DECIGO          -> decigoPsd fin


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
    toLog z = log (z/f0)
    psdmodel y = (6.499*1.0E-25*(9.72*10e-9*(exp (-1.43-9.88*y-0.23*y**2))+1.17*(exp (0.14-3.10*y-0.26*y**2))+1.70*(exp (0.14+1.09*y-0.013*y**2))+1.25*(exp (0.071+2.83*y-4.91*y**2))))**2


advirgoPsd :: Vector Double -> Vector Double
advirgoPsd fin = mapVector psdmodel x
  where
    f0 = 300 :: Double
    x  = mapVector toLog fin
    toLog z = log (z/f0)
    psdmodel y = (1.259*1.0E-24*(0.07*(exp (-0.142-1.437*y+0.407*y**2))+3.10*(exp (-0.466-1.043*y-0.548*y**2))+0.40*(exp (-0.304+2.896*y-0.293*y**2))+0.09*(exp (1.466+3.722*y-0.984*y**2))))**2

etPsd :: Vector Double -> Vector Double
etPsd fin = mapVector psdmodel x
  where
    f0 = 100.0
    s0 = 1e-50
    a1 = 2.39e-27
    a2 = 0.349
    a3 = 1.76
    a4 = 0.409
    b1 = -15.64
    b2 = -2.145
    b3 = -0.12
    b4 = 1.10
    x = mapVector (/f0) fin
    psdmodel y = s0 * (a1 * y**b1 + a2 * y**b2 + a3 * y**b3 + a4 * y**b4)**2

elisaPsd :: Vector Double -> Vector Double
elisaPsd fin = mapVector psdmodel fin
  where
    c = 2.9979e8 -- [m/s]
    l = 1e9 -- [m]
    sacc y = 2.13e-29 * (1.0 + 1e-4/y)
    ssn = 5.25e-23
    somn = 6.28e-23
    psdmodel y = (20.0/3.0) * (4.0*(sacc y)/(2.0*pi*y)**4.0 + ssn + somn)/l**2.0 * (1.0 + (y / (0.41*c/(2.0*l) ) ) )**2.0

decigoPsd :: Vector Double -> Vector Double
decigoPsd fin = mapVector psdmodel fin
  where
    fc = 7.69
    psdmodel y = 3.30e-50*y**(-4.0) + 3.09e-47*(1.0 + (y/fc)**2.0)

