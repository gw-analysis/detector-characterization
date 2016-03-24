
import qualified Data.Vector.Storable as V
import qualified HasKAL.DetectorUtils.Detector as D
import HasKAL.FrameUtils.Function (readFrameWaveData')
import HasKAL.SignalProcessingUtils.Resampling (downsampleWaveData, downsampleSV)
import HasKAL.WaveUtils.Data
import System.Environment (getArgs)

-- readFrameWaveData' :: HDD.Detector
--                   -> String
--                   -> String
--                   -> IO (Maybe HWD.WaveData)
-- readFrameWaveData' detector channel fname = runMaybeT $ MaybeT $ do

-- downsampleWaveData :: Double -> WaveData -> WaveData
-- downsampleWaveData newfs x = y

main = do
 args <- getArgs
 let fname | length args == 1 = head args
           | otherwise = error "Usage: debugResampling gwffile"
-- let fname = "../H-H1_LOSC_4_V1-1126259446-32.gwf"
 w <- readFrameWaveData' D.LIGO_Hanford "H1:LOSC-STRAIN" fname
 case w of 
  Nothing -> error "No valid input data"
  Just w' -> do
    let w'' = downsampleWaveData 1024 w'
        w'''= downsampleSV 4096 1024 (gwdata w')
        len = V.length (gwdata w')
        len'= V.length (gwdata w'')
        len''= V.length w'''
    print len
    print len'
    print len''


