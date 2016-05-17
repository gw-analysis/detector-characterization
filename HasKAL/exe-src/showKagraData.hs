
import qualified Data.Vector.Storable as V
import HasKAL.DataBaseUtils.FrameFull.Function (kagraWaveDataGetC)
import HasKAL.SignalProcessingUtils.Resampling (downsampleWaveData, resampleWaveData)
import HasKAL.TimeUtils.Function (deformatGPS)
import HasKAL.TimeUtils.Signature
import HasKAL.WaveUtils.Data (WaveData(..))
import System.Console.GetOpt
import System.Environment (getArgs)
import System.IO (stdout, hPutStrLn)


main = do
  getArgs >>= \args' -> case options args' of
    Right (ss, args) -> case ss of
      [] -> do let method = doDownSample
               go method args
      _  -> do let method = doReSample
               go method args
  where 
    go method args = case (length args) of
        4 -> do let (ch, fsfact', gps', duration') = (head args, args!!1, args!!2, args!!3)
                    gps = read gps' :: Int
                    duration = read duration' :: Int
                    fsfact = read fsfact' :: Int
                kagraWaveDataGetC gps duration ch >>= \judge -> case judge of
                  Nothing  -> putStrLn "No data found."
                  Just wav -> do let td = concatMap (timendat . method fsfact) wav
                                 mapM_ (\(t,x) -> hPutStrLn stdout $ (show t)++" "++show x) td
        _ -> error "Usage: showKagraData [-r(--resampleonly)] chname downsamplefactor gps duration"


timendat y = let t = deformatGPS $ startGPSTime y
                 fs = samplingFrequency y
                 tl = [t+i/fs|i<-[0.0,1.0..fromIntegral (length xl) -1.0]] 
                 xl = V.toList $ gwdata y
              in zip tl xl


doDownSample fsfact w = do let fs = samplingFrequency w
                           case fsfact > 1 of
                             False -> w
                             True  -> downsampleWaveData (fs/fromIntegral fsfact) w


doReSample fsfact w = do let fs = samplingFrequency w
                         case fsfact > 1 of
                           False -> w
                           True  -> resampleWaveData (fs/fromIntegral fsfact) w


data Flag = ResampleOnly deriving Show


options :: [String] -> Either String ([Flag],[String])
options args = case getOpt RequireOrder [Option ['r'] ["resampleonly"] (NoArg ResampleOnly) "resample only"] args of
  (ss, as, []) -> Right (ss, as)
  (_, _, es) -> Left $ concat es











