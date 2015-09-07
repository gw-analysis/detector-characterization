import HasKAL.FrameUtils.FrameUtils
import HasKAL.SpectrumUtils.GwPsdMethod
import HasKAL.SpectrumUtils.SpectrumUtils
import Data.List

--import HasKAL.MonitorUtils.RangeMon.StochMon.StochMon
import StochMon
import HasKAL.DetectorUtils.Detector

main = do
  let fname = "/home/yokozawa/L-L1_RDS_C03_L2-877264406-128.gwf"
      chname = "L1:LSC-STRAIN"
  fdata <- readFrame chname (fname)

  let ts = map realToFrac (eval fdata)
      psdtuple = gwpsd ts 16384 16384
      obst = 3.0*365.0*86400.0
      smp = makeStochMonParam KAGRA obst LIGO_Hanford psdtuple 1.0 1000.0 1.0 0.05 0.95 
      printshow xx = putStrLn $ (show (fst xx)) ++ " " ++ (show (snd xx))
--  mapM_ printshow $ h2omega_param smp
  mapM_ printshow $ h2omega_adet smp





     




