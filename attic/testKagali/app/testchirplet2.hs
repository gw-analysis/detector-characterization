

import qualified Data.Vector.Storable as V
import HasKAL.IOUtils.Function(loadASCIIdataCV)
import HasKAL.MonitorUtils.ChirpletMon.Data
import HasKAL.MonitorUtils.ChirpletMon.Function (catChirpletGram, chirpletTrainWave)
import HasKAL.MonitorUtils.ChirpletMon.Plot (plotChirpletGram, plotChirpletGram2png)
import HasKAL.SignalProcessingUtils.Resampling (downsampleSV)
import HasKAL.SimulationUtils.Injection.Function (addInjsig)
import HasKAL.WaveUtils.Data
import qualified Numeric.LinearAlgebra as N


main = do
  let v = loadASCIIdataCV "dat/KRD_SFHx_ascii.dat"
      fs = 2048 :: Double
      fsorig = 16384 :: Double
      htv = head $ V.toList $ head v
      hp = downsampleSV fsorig fs $ v !! 1
      hc = downsampleSV fsorig fs $ v !! 2
      ts' = N.scale 1E-23 $ N.randomVector 1 N.Gaussian (V.length hp) :: V.Vector Double
      ts = addInjsig 0 ts' hp
      w = vec2wave fs htv ts
      p = ChirpletParam
        { alpha = 5 :: Double
        , ipath = 4 :: Int
        }
      dt = (2.0**7.0+0.5)/fs
      odt = dt/2
      x = catChirpletGram $ chirpletTrainWave p dt odt w
      plotparam = ChirpletPlotParam
        { xrange = (0.03, 0.35) :: (Double, Double)
        , yrange = (10, 1000) :: (Double, Double)
        }
  plotChirpletGram2png x plotparam "SN_chirplet2.png"
