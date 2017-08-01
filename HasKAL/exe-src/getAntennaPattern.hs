

import HasKAL.DetectorUtils.Detector
import HasKAL.DetectorUtils.Function
import HasKAL.DetectorUtils.DetectorParam
import HasKAL.IOUtils.Function

import System.Console.GetOpt
import System.Environment (getArgs)
import System.IO (stdout, hPutStrLn)
import System.IO.Unsafe (unsafePerformIO)


main = do
  varArgs <- getArgs
  (det', phi', theta', psi') <- case length varArgs of
    4 -> return (varArgs!!0, varArgs!!1, varArgs!!2, varArgs!!3)
    _ -> error "Usage: getAntennaPattern det phi theta psi"


  {-- parameters --}
  let det = read det' :: Detector
      phi = read phi' :: Double
      psi = read psi' :: Double
      theta = read theta' :: Double

  let fpct | det==LIGO_Livingston = fplusfcrossts ligoLivingston phi theta psi
           | det==LIGO_Hanford    = fplusfcrossts ligoHanford phi theta psi
           | det==VIRGO           = fplusfcrossts virgo phi theta psi
           | det==KAGRA           = fplusfcrossts kagra phi theta psi
      pol = fst fpct
      delay = snd fpct

  mapM_ (\(x,y,z) -> hPutStrLn stdout $ (x++" "++y++" "++z))
               $ [("h+", "hx", "delay[s]"),(show $ fst pol, show $ snd pol, show delay)]
