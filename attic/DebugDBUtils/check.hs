
import System.Environment (getArgs)
import qualified Data.Vector.Storable as V (length, (!))

import HasKAL.TimeUtils.Function (deformatGPS)
import HasKAL.TimeUtils.GPSfunction (time2gps)
import HasKAL.WaveUtils.Function (mergeOverlapWaveDataC)
import HasKAL.WaveUtils.Data (WaveData(..))
import HasKAL.DetectorUtils.Detector (Detector(..))
-- import HasKAL.DataBaseUtils.XEndEnv.Function (kagraWaveDataGet, kagraWaveDataGetC, kagraWaveDataGet0)
import XEndEnvFunction (kagraWaveDataGet, kagraWaveDataGetC, kagraWaveDataGet0)

main = do
  args <- getArgs
  (year, month, day) <- case length args of
                         3 -> return (args!!0, show0 2 (args!!1), show0 2 (args!!2))
                         _ -> error "Usage: TimeSeries yyyy mm dd"

  {-- parameters --}
  let ch = "K1:PEM-EX_ACC_NO2_X_FLOOR"
      gps = read $ time2gps $ year++"-"++month++"-"++day++" 00:00:00 JST"
      dur = 86400

  {-- read data --}
  mbWd <- kagraWaveDataGet (fromIntegral gps) (fromIntegral dur) ch
  mbWdC <- kagraWaveDataGetC (fromIntegral gps) (fromIntegral dur) ch
  mbWd0 <- kagraWaveDataGet0 (fromIntegral gps) (fromIntegral dur) ch
  let (wd, wdC, wdC', wd0) = case (mbWd, mbWdC, mbWd0) of
              (Nothing,_ , _) -> error $ "Can't find file: "++year++"/"++month++"/"++day
              (_, Nothing, _) -> error $ "Can't find file: "++year++"/"++month++"/"++day
              (_, _, Nothing) -> error $ "Can't find file: "++year++"/"++month++"/"++day
              (Just x, Just y, Just z) -> (x, y, mergeOverlapWaveDataC y, z)

  {-- main --}
  putStrLn $ year ++ "/" ++ month ++ "/" ++ day
  putStrLn $ "   " ++ show (gps, 0) ++ " ~ " ++ show (gps+86400, 0)

  putStrLn "\nNum of Data"
  putStrLn . ("   kagraWaveDataGet  : "++) . show . V.length . gwdata $ wd
  putStrLn . ("   kagraWaveDataGetC : "++) . show . sum . map (V.length . gwdata) $ wdC
  putStrLn . ("   kagraWaveDataGetC': "++) . show . sum . map (V.length . gwdata) $ wdC'
  putStrLn . ("   kagraWaveDataGet0 : "++) . show . V.length . gwdata $ wd0
  
  putStrLn "\nStart GPS"
  putStrLn . ("   kagraWaveDataGet  : "++) . show . startGPSTime $ wd
  putStrLn . ("   kagraWaveDataGetC : "++) . show . startGPSTime . head $ wdC
  putStrLn . ("   kagraWaveDataGetC': "++) . show . startGPSTime . head $ wdC'
  putStrLn . ("   kagraWaveDataGet0 : "++) . show . startGPSTime $ wd0

  putStrLn "\nStop GPS"
  putStrLn . ("   kagraWaveDataGet  : "++) . show . stopGPSTime $ wd
  putStrLn . ("   kagraWaveDataGetC : "++) . show . stopGPSTime . last $ wdC
  putStrLn . ("   kagraWaveDataGetC': "++) . show . stopGPSTime . last $ wdC'
  putStrLn . ("   kagraWaveDataGet0 : "++) . show . stopGPSTime $ wd0

  putStrLn "\nList of StartGPS ~ StopGPS (kagraWaveDataGetC)"
  mapM (\x -> putStrLn . ("   "++) $ (show $ startGPSTime x) ++  " ~ " ++ (show $ stopGPSTime x)
              ++ " = " ++ (show $ (fst $ stopGPSTime x) - (fst $ startGPSTime x)) ++ "   " ++ (show $ V.length $ gwdata x)
       ) wdC

  putStrLn "\nList of StartGPS ~ StopGPS (kagraWaveDataGetC')"
  mapM (\x -> putStrLn . ("   "++) $ (show $ startGPSTime x) ++  " ~ " ++ (show $ stopGPSTime x)
              ++ " = " ++ (show $ (fst $ stopGPSTime x) - (fst $ startGPSTime x)) ++ "   " ++ (show $ V.length $ gwdata x)
       ) wdC'

  putStrLn "\nData Value"
  print $ map (\i -> (gwdata wd)V.!i) [3000..3004]
  print $ map (\i -> (gwdata $ head wdC)V.!i) [3000..3004]
  print $ map (\i -> (gwdata $ head wdC')V.!i) [3000..3004]
  print $ map (\i -> (gwdata wd0)V.!i) [3000..3004]

  putStrLn ""
  print $ map (\i -> (gwdata wd)V.!(V.length (gwdata wd) - 1 - i)) [0..4]
  print $ map (\i -> (gwdata $ last wdC)V.!(V.length (gwdata $ last wdC) - 1 - i)) [0..4]
  print $ map (\i -> (gwdata $ last wdC')V.!(V.length (gwdata $ last wdC') - 1 - i)) [0..4]
  print $ map (\i -> (gwdata wd0)V.!(V.length (gwdata wd0) - 1 - i)) [0..4]

  
{-- Internal Functions --}
show0 :: Int -> String -> String
show0 digit number
  | len < digit = (concat $ replicate (digit-len) "0") ++ number
  | otherwise   = number
  where len = length number

