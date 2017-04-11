
import Data.List.Split
import qualified Data.Vector.Storable as V
import HasKAL.DataBaseUtils.FrameFull.Function (kagraWaveDataGetC)
import HasKAL.SignalProcessingUtils.Resampling (downsampleWaveData,resampleWaveData)
import HasKAL.TimeUtils.Function (deformatGPS)
import HasKAL.TimeUtils.GPSfunction (time2gps)
import HasKAL.TimeUtils.Signature
import HasKAL.WaveUtils.Data (WaveData(..))
import System.Console.GetOpt
import System.Environment (getArgs)
import System.IO (stdout, hPutStrLn, hFlush)


main = do
  (varOpt, varArgs) <- getArgs >>= \optargs ->
    case getOpt Permute options optargs of
      (opt, args,[]) -> return (Prelude.foldl (flip id) defaultOptions opt, args)
      (_  , _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
        where header = "Usage: showKagraData [-r(--resample) P/Q, -l] channel gps[localtime] duration"

  case (length varArgs) of
    3 -> do let (ch,  gps', duration') = (head varArgs, varArgs!!1, varArgs!!2)
                duration = read duration' :: Int
                gps = case optLocaltime varOpt of
                        False -> read (varArgs!!1) :: Int
                        True -> read (time2gps (varArgs!!1)) :: Int
            kagraWaveDataGetC gps duration ch >>= \judge -> case judge of
              Nothing  -> putStrLn "No data found."
              Just wav -> do
                let f = case optResample varOpt of
                          [] -> timendat
                          pq -> timendat . (doReSample (p,q))
                                  where (p,q) = getPQ pq ::(Int,Int)
                    td = concatMap f wav
                mapM_ (\(t,x) -> hPutStrLn stdout $ (show t)++" "++show x) td
    _ -> error "Usage: showKagraData [-r(--resample) P/Q, -l(localtime)] chname gps duration"


timendat y = let t = deformatGPS $ startGPSTime y
                 fs = samplingFrequency y
                 tl = [t+i/fs|i<-[0.0,1.0..fromIntegral (length xl) -1.0]]
                 xl = V.toList $ gwdata y
              in zip tl xl


doReSample (p,q) w = do let fs = samplingFrequency w
                        case p == 1 of
                          True  -> downsampleWaveData (fs/fromIntegral q) w
                          False -> resampleWaveData (p,q) w


getPQ :: String
      -> (Int, Int)
getPQ x = let a = take 2 $ map (\y-> read y :: Int) $ splitOn "/" x
           in (head a,a!!1)


data Options = Options
  { optResample  :: [Char]
  , optLocaltime :: Bool
  } deriving (Show)


defaultOptions  = Options
  { optResample = []
  , optLocaltime = False
  }


options :: [OptDescr (Options -> Options)]
options =
  [ Option ['r'] ["resample"]
      ( ReqArg (\ pq opts -> opts { optResample = pq}) "P/Q" )
      "resampling factor P/Q"
  , Option ['l'] ["localtime"]
      ( NoArg (\ opts -> opts {optLocaltime = True}))
      "showKagraData -l channel \"2017-01-01 00:00:00 JST\" duration"
  ]
