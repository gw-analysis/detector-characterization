
import qualified Data.Vector.Storable as V
import HasKAL.DataBaseUtils.FrameFull.Function (kagraWaveDataGetC)
import HasKAL.SignalProcessingUtils.Resampling (downsampleWaveData)
import HasKAL.TimeUtils.Function (deformatGPS)
import HasKAL.TimeUtils.Signature
import HasKAL.WaveUtils.Data (WaveData(..))
import System.Environment (getArgs)
import System.IO (stdout, hPutStrLn)

main = do
  (ch, fsfact', gps', duration') <- getArgs >>= \args-> case (length args) of
    4 -> return (head args, args!!1, args!!2, args!!3)
    _ -> error "Usage: showKagraData chname downsamplefactor gps duration"
  let gps = read gps' :: Int
      duration = read duration' :: Int
      fsfact = read fsfact' :: Int
  kagraWaveDataGetC gps duration ch >>= \judge -> case judge of
    Nothing  -> putStrLn "No data found."
    Just wav -> do let td = concatMap (timendat . doDownSample fsfact) wav
                   mapM_ (\(t,x) -> hPutStrLn stdout $ (show t)++" "++show x) td


timendat y = let t = deformatGPS $ startGPSTime y
                 fs = samplingFrequency y
                 tl = [t+i/fs|i<-[0.0,1.0..fromIntegral (length xl) -1.0]] 
                 xl = V.toList $ gwdata y
              in zip tl xl


doDownSample fsfact w = do let fs = samplingFrequency w
                           case fsfact > 1 of
                             False -> w
                             True  -> downsampleWaveData (fs/fromIntegral fsfact) w


