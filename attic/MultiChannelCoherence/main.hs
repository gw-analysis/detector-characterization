
import Control.Monad (liftM)
import Data.Maybe (catMaybes)
import Data.Complex (magnitude)
import qualified Data.Vector.Storable as V

import HasKAL.DataBaseUtils.XEndEnv.Function (kagraWaveDataGet)
import HasKAL.DetectorUtils.Detector (Detector(..))
import HasKAL.WaveUtils (WaveData(..))
import HasKAL.PlotUtils.HROOT.PlotGraph
import HasKAL.MonitorUtils.CoherenceMon.Function (multiCoherenceW)

main = do
  let fname = "/data/kagra/xend/R0206/K-K1_R-1120443552-32.gwf"
      ch'y = "K1:PEM-EX_MIC_FLOOR"
      ch'x = ["K1:PEM-EX_ACC_NO2_X_FLOOR"
             ,"K1:PEM-EX_ACC_NO2_Y_FLOOR"
             ,"K1:PEM-EX_ACC_NO2_Z_FLOOR"
             ,"K1:PEM-EX_MAG_X_FLOOR"
             ,"K1:PEM-EX_MAG_Y_FLOOR"
             ,"K1:PEM-EX_MAG_Z_FLOOR"
             ]

      n = 1
      duration = 1

  mby <- kagraWaveDataGet 1121094017 duration ch'y KAGRA
  mbx <- liftM catMaybes $ mapM (\c -> kagraWaveDataGet 1121094017 128 c KAGRA) ch'x

  (yoft, xioft) <- case (mby, mbx) of
                    (Nothing, _) -> error "Can't find data."
                    (_, [])      -> error "Can't find data."
                    (Just y,  xs) -> return (y, xs)
  
  let (fvec, coh, alpha) = multiCoherenceW ((*) n $ truncate $ samplingFrequency yoft) yoft xioft
  oPlotV LogY Line 1 [BLUE,RED,GREEN,CYAN,YELLOW,PINK] ("freq [Hz]", "|#alpha(f)|") 0.05 "MIC vs. ACC + MAG"
    "Alpha.png" ((0,0),(1e-4,1e1)) $ map (\x -> (fvec, V.map magnitude x)) alpha
  plotV LogY Line 1 BLUE ("freq [Hz]", "Coh(f)") 0.05 "MIC vs. ACC + MAG"
    "Coef.png" ((0,0),(1e-2,10)) $ (fvec, coh)

  let (fvec, coh1, alpha1) = multiCoherenceW ((*) n $ truncate $ samplingFrequency yoft) yoft $ take 3 xioft
  oPlotV LogY Line 1 [BLUE,RED,GREEN] ("freq [Hz]", "|#alpha(f)|") 0.05 "MIC vs. ACC"
    "Alpha-1.png" ((0,0),(1e-4,1e1)) $ map (\x -> (fvec, V.map magnitude x)) alpha1
  plotV LogY Line 1 BLUE ("freq [Hz]", "Coh(f)") 0.05 "MIC vs. ACC"
    "Coef-1.png" ((0,0),(1e-2,2)) $ (fvec, coh1)

  let (fvec, coh2, alpha2) = multiCoherenceW ((*) n $ truncate $ samplingFrequency yoft) yoft $ drop 3 xioft
  oPlotV LogY Line 1 [CYAN,YELLOW,PINK] ("freq [Hz]", "|#alpha(f)|") 0.05 "MIC vs. MAG"
    "Alpha-2.png" ((0,0),(1e-4,1e1)) $ map (\x -> (fvec, V.map magnitude x)) alpha2
  plotV LogY Line 1 BLUE ("freq [Hz]", "Coh(f)") 0.05 "MIC vs. MAG"
    "Coef-2.png" ((0,0),(1e-2,2)) $ (fvec, coh2)

