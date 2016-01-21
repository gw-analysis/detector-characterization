{- |
Module      : main
Description : This is documentation tests.
Copyright   : (c) WhoAmI, 2014
License     : ???
Maintainer  : Takahiro Yamamoto %mail%
Stability   : test
Portability : POSIX

-}{-
  * Last Modified: 2016/01/22 02:13:42
-}

import HasKAL.FrameUtils.Function (readFrameWaveData')
import HasKAL.DetectorUtils.Detector (Detector(..))
import HasKAL.WaveUtils (WaveData(..))
import Function (multiCoherenceW)
import Control.Monad (liftM)
import Data.Maybe (catMaybes)
import HasKAL.PlotUtils.HROOT.PlotGraph
import Data.Complex (magnitude)
import qualified Data.Vector.Storable as V

main = do
  let fname = "/data/kagra/xend/R0206/K-K1_R-1120443552-32.gwf"
      ch'y = "K1:PEM-EX_ACC_NO2_X_FLOOR"
      ch'x = ["K1:PEM-EX_ACC_NO2_Y_FLOOR"
             ,"K1:PEM-EX_ACC_NO2_Z_FLOOR"
             ,"K1:PEM-EX_MAG_X_FLOOR"
             ,"K1:PEM-EX_MAG_Y_FLOOR"
             ,"K1:PEM-EX_MAG_Z_FLOOR"
             ,"K1:PEM-EX_MIC_FLOOR"]


  mby <- readFrameWaveData' KAGRA ch'y fname
  mbx <- liftM catMaybes $ mapM (\c -> readFrameWaveData' KAGRA c fname) ch'x :: IO [WaveData]

  (yoft, xioft) <- case (mby, mbx) of
                    (Nothing, _) -> error "Can't find data."
                    (_, [])      -> error "Can't find data."
                    (Just y,  xs) -> return (y, xs)
  
  let (fvec, coh, alpha) = multiCoherenceW (truncate $ samplingFrequency yoft) yoft xioft
  plotXV Linear Line 1 BLUE ("freq [Hz]", "Coh(f)") 0.05 "multi-ch. Coherence" 
    ((0,0),(-0.05,1.05)) $ (fvec, V.map magnitude coh)

