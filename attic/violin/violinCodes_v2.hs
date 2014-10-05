



import Numeric.LinearAlgebra
import HasKAL.MonitorUtils.RangeMon.InspiralRingdownSnrQuanta
import HasKAL.MonitorUtils.RangeMon.InspiralRingdownDistanceQuanta
import HasKAL.MonitorUtils.RangeMon.IMBH
import qualified HasKAL.PlotUtils.HROOT.PlotGraph as HR
import qualified HasKAL.PlotUtils.HROOT.AppendFunctionHROOT as HAF
import HasKAL.SpectrumUtils.DetectorSensitivity
import System.Environment (getArgs)

main :: IO()
main = do
  len <- getArgs
  mapM_ go len

go :: String -> IO()
go length' = do
  let length = read length' :: Double
  let freq = [10..1000] :: [Double]
      freqV = fromList freq
      {- Violin mode psd -}
      v = map (\x->(gf length x)**2) freq
      vV=fromList v
      {- KAGRA psd-}
      kagraPsd = ifonoisepsd KAGRA freqV
      {- KAGRA psd Violin mode added -}
      wa = kagraPsd + vV
      {- format psd data -}
      psddatOnlyV = zip freq v
      psddatV = zip freq (toList wa)
      psddat  = zip freq (toList kagraPsd)

      {- insp snr changing mass -}
      mvecI = [1.4] ++ [5, 10..150]
      mvecR = [1.4] ++ [5, 15..1000]
      mvecIMBH = [10, 20..400]


      distIdat = zip mvecI $ map (\m->distInspiral m m psddat) mvecI
      distIdatV= zip mvecI $ map (\m->distInspiral m m psddatV) mvecI
      distIratio=zip mvecI $ map (\m->(distInspiral m m psddatV)/(distInspiral m m psddat)) mvecI

      distRdat = zip mvecR $ map (\m->distRingdown m psddat) mvecR
      distRdatV= zip mvecR $ map (\m->distRingdown m psddatV) mvecR
      distRratio=zip mvecR $ map (\m->(distRingdown m psddatV)/(distRingdown m psddat)) mvecR

      distIMBHdat = zip mvecIMBH $ map (\m->distImbh m m psddat) mvecIMBH
      distIMBHdatV = zip mvecIMBH $ map (\m->distImbh m m psddatV) mvecIMBH
      distIMBHRatio = zip mvecIMBH $ map (\m->(distImbh m m psddatV)/(distImbh m m psddat)) mvecIMBH


  {- plotting -}
  -- set plotting parameter
  let ext = ".eps"
      fname1 = "20141005_violinOnly_psd_"++length'++ext
      fname2 = "20141005_violin_psd_"++length'++ext
      fname3 = "20141005_mass-dist_Insp_ratio"++length'++ext
      fname4 = "20141005_mass-dist_Insp_"++length'++ext
      fname5 = "20141005_mass-dist_Ring_ratio"++length'++ext
      fname6 = "20141005_mass-dist_Ring_"++length'++ext
      fname7 = "20141005_mass-dist_IMBH_ratio"++length'++ext
      fname8 = "20141005_mass-dist_IMBH_"++length'++ext

  HAF.addSignalHandle
  HR.plot HR.LogXY HR.Line 2 ("frequency", "Spectrum") 0.05 "1st_violinOnly psd"
    fname1 ((0,0), (0,0))
    $ psddatOnlyV
  HR.plot HR.LogXY HR.Line 2 ("frequency","Spectrum") 0.05 " "
    fname2 ((0,0),(0,0))
    $ psddatV
  HR.plot HR.Linear HR.Line 2 ("mass","distance") 0.05 "Inspiral mass-distance"
    fname3 ((0,0),(0,0))
    $ distIratio
  HR.oPlot HR.LogY HR.Line 2 ("mass","distance") 0.05 "Inspiral mass-distance"
    fname4 ((0,0),(0,0))
    $ [distIdat, distIdatV]
  HR.plot HR.Linear HR.Line 2 ("mass","distance") 0.05 "Ringdown mass-distance"
    fname5 ((0,0),(0,0))
    $ distRratio
  HR.oPlot HR.LogY HR.Line 2 ("mass","distance") 0.05 "Ringdown mass-distance"
    fname6 ((0,0),(0,0))
    $ [distRdat, distRdatV]
  HR.plot HR.Linear HR.Line 2 ("mass","distance") 0.05 "IMBH mass-distance"
    fname7 ((0,0),(0,0))
    $ distIMBHRatio
  HR.oPlot HR.LogY HR.Line 2 ("mass","distance") 0.05 "IMBH mass-distance"
    fname8 ((0,0),(0,0))
    $ [distIMBHdat, distIMBHdatV]






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
  | len==300 = mkViolin 220 (2*76488.6) 9.6E6
  | len==320 = mkViolin 200 (2*68639.9) 9.7E6
  | len==350 = mkViolin 175 (2*59057.8) 1.0E7
  | len==400 = mkViolin 143 (2*47201.1) 1.0E7
  | len==450 = mkViolin 120 (2*38732.9) 1.1E7
  | len==500 = mkViolin 103 (2*32452.8) 1.1E7

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



