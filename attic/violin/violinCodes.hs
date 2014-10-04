



import Numeric.LinearAlgebra
import HasKAL.MonitorUtils.RangeMon.InspiralRingdownSnrQuanta
import HasKAL.MonitorUtils.RangeMon.InspiralRingdownDistanceQuanta
import qualified HasKAL.PlotUtils.HROOT.PlotGraph as HR
import HasKAL.SpectrumUtils.DetectorSensitivity


main :: IO()
main = do
  let length = 300 :: Double
      freq = [10..1000] :: [Double]
      freqV = fromList freq
      {- Violin mode psd -}
      v = map (\x->(gf length x)**2) freq
      vV=fromList v
      {- KAGRA psd-}
      kagraPsd = ifonoisepsd KAGRA freqV
      {- KAGRA psd Violin mode added -}
      wa = kagraPsd + vV
      {- format psd data -}
      psddatV = zip freq (toList wa)
      psddat  = zip freq (toList kagraPsd)

      {- insp snr changing mass -}
      mvecI = [1.4] ++ [5, 10..150]
      mvecR = [1.4] ++ [5, 15..1000]

      distIdat = zip mvecI $ map (\m->distInspiral m m psddat) mvecI
      distIdatV= zip mvecI $ map (\m->distInspiral m m psddatV) mvecI
      distIratio=zip mvecI $ map (\m->(distInspiral m m psddatV)/(distInspiral m m psddat)) mvecI

      distRdat = zip mvecR $ map (\m->distRingdown m psddat) mvecR
      distRdatV= zip mvecR $ map (\m->distRingdown m psddatV) mvecR
      distRratio=zip mvecR $ map (\m->(distRingdown m psddatV)/(distRingdown m psddat)) mvecR


--  print $ snrInspiral 1.4 1.4 200 psddatV
--  print $ (snrInspiral 1.4 1.4 200 psddatV)/(snrInspiral 1.4 1.4 200 psddat)



  {- plot spectrum -}
  HR.plot HR.LogXY HR.Line ("frequency","Spectrum") "violin psd" "violin_psd_300.png" ((0,0),(0,0))
      $ psddatV
  HR.plot HR.Linear HR.Line ("mass","distance") "Inspiral mass-distance" "mass-dist_Insp_300_ratio.png" ((0,0),(0,0))
      $ distIratio
  HR.plot HR.Linear HR.Line ("mass","distance") "Ringdown mass-distance" "mass-dist_Ring_300_ratio.png" ((0,0),(0,0))
      $ distRratio
  HR.oPlot HR.LogY HR.Line ("mass","distance") "Inspiral mass-distance"
      "mass-dist_Insp_300.png" ((0,0),(0,0)) [distIdat, distIdatV]
  HR.oPlot HR.LogY HR.Line ("mass","distance") "Ringdown mass-distance"
      "mass-dist_Ring_300.png" ((0,0),(0,0)) [distRdat, distRdatV]






-- calculation of 1st violin mode for KAGRA
-- reference :
-- p34, Eq(3.41) in http://t-munu.phys.s.u-tokyo.ac.jp/theses/yamamoto_d.pdf
-- p31 in
-- http://www.gravity.ircs.titech.ac.jp/GWADW2014/slide/Kazuhiro_Yamamoto.pdf
--

{- the spectrum of the thermal noise of the structure damping -}
gf :: Double -> Double -> Double
gf len f = (sqrt 4/3000.0)*sqrt (4*kB*temp/((massV params)*(qfactor params)*omega)
         * omega0**2 / ((omega**2-omega0**2)**2 + omega0**4/(qfactor params)**2))
  where params = getViolinParams len
        omega = 2*pi*f
        omega0 = 2*pi*(fstVf params)
        temp = 20.0
        kB = 1.38E-23



{- parameter setting -}
getViolinParams :: Double -> Violin
getViolinParams len
  | len==300 = mkViolin 220 76488.6 9.6E6
  | len==320 = mkViolin 200 68639.9 9.7E6
  | len==350 = mkViolin 175 59057.8 1.0E7
  | len==400 = mkViolin 143 47201.1 1.0E7
  | len==450 = mkViolin 120 38732.9 1.1E7
  | len==500 = mkViolin 103 32452.8 1.1E7

data Violin = Violin
  { fstVf :: Double
  , massV :: Double
  , qfactor :: Double
  }

mkViolin :: Double -> Double -> Double -> Violin
mkViolin vf1 vm vQ =
  Violin { fstVf = vf1
         , massV = vm
         , qfactor=vQ}



