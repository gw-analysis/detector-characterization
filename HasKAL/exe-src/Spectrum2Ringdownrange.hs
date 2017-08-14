
import qualified Data.Vector.Storable as V

import HasKAL.IOUtils.Function
import HasKAL.MonitorUtils.RangeMon.InspiralRingdownDistanceQuanta
import HasKAL.PlotUtils.HROOT.PlotGraph
import System.Console.GetOpt
import System.Environment (getArgs)
import System.IO (stdout, hPutStrLn)

main = do
  (snr', asdfile) <- getArgs >>= \varArgs -> case (length varArgs) of
    2 -> return (head varArgs, varArgs!!1)
    _ -> error "Usage: Spectrum2Ringdownrange SNR ASDfile"


  let snr = read snr' :: Double
      asddat = loadASCIIdataCV asdfile
      fre = V.toList $ head asddat
      asd = V.toList $ (asddat !! 1)
      spec = zip fre (map (\x->x**2) asd)
      mass = [1.4,2.4..100]
      dist = map (\x->distRingdownCore x snr 0.98 0.03 0.0 spec 4000 10) mass
      title = "Ringdown range"

  plotX Linear Line 1 BLUE ("mass[msolar]", "range[Mpc]") 0.05 title ((0,0),(0,0)) $ zip mass dist
