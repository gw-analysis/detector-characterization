import HasKAL.FrameUtils.FrameUtils
import HasKAL.SpectrumUtils.GwPsdMethod
import HasKAL.SpectrumUtils.SpectrumUtils
import HasKAL.PlotUtils.PlotOption.PlotOptionHROOT
import qualified HasKAL.PlotUtils.HROOT.PlotGraph as HR
import Data.List

--import HasKAL.MonitorUtils.RangeMon.StochMon.StochMon
import StochMon
import HasKAL.DetectorUtils.Detector

main = do
  let gtime = 877232267
  let fname = "/data/L-L1_RDS_C03_L2-8772/L-L1_RDS_C03_L2-"++(show gtime)++"-128.gwf"
  ch <- getChannelList fname
  let [(chname, fs)]=ch
  fdata <- readFrame chname (fname)

  let y = map realToFrac (eval fdata)
      psdtuple = gwpsd y 16384 16384

  let h2omegaa = h2omega_sens_allf 100 KAGRA VIRGO 0.05 0.95 [1..1000]
      h2omega = map (h2omega_sens_gui VIRGO) [(100.0, 1.0E-40),(105, 2.0E-40)]
      tttt = head $ map (h2omega_sens_gui VIRGO) [(100.0, 1.0E-40),(105, 2.0E-40)]
  HR.plot HR.LogXY HR.Line ("time[s]","Strain") "test" "test100.eps" $ zip [1..1000] h2omega





--  let fname = "/data/L-L1_RDS_C03_L2-8772/L-L1_RDS_C03_L2-877231627-128.gwf"
--  ch <- getChannelList fname
--  let chname = map fst ch
--  mapM_ putStrLn chname
--  mapM_ putStrLn fs
--     fdata <- readFrame "H1:LSC-STRAIN" "H-H2_RDS_C03_L2-877298655-128.gwf"
--     let dat   = map realToFrac (eval fdata)
--         lstxt = take (length dat) [1,2..]

--     let lsty  = map snd $ gwpsd dat (163) 16384
--         lstxf = map fst $ gwpsd dat (163) 16384
--     mapM_ putStrLn (map show (gwpsd dat 163 16384))
     




