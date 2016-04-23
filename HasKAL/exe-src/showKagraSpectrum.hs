
import Data.List (maximum)
import qualified Data.Vector.Storable as V
import HasKAL.DataBaseUtils.FrameFull.Function (kagraWaveDataGetC)
import HasKAL.SpectrumUtils.Function (mapSpectrum)
import HasKAL.SpectrumUtils.SpectrumUtils (gwOnesidedPSDWaveData)
import HasKAL.WaveUtils.Data (WaveData(..))
import System.IO (stdout, hPutStrLn)
import System.Environment (getArgs)


main = do
  {-- parameters --}
  args <- getArgs
  (ch, gps', duration', dtFFT') <- case (length args) of
   4 -> return (head args, args!!1, args!!2, args!!3)
   _ -> error "Usage: showKagraSpectrum channel gps duration dtFFT"

  {-- read data --}
  let gps = read gps' :: Int
      duration = read duration' :: Int
      dtFFT = read dtFFT'
  kagraWaveDataGetC gps duration ch >>= \judge -> case judge of
    Nothing  -> putStrLn "No data found."
    Just wav -> do
     case length wav of
      1 -> do let (vf,vasd) = mapSpectrum sqrt $ gwOnesidedPSDWaveData dtFFT $ head wav
              mapM_ (\(t,x) -> hPutStrLn stdout $ (show t)++" "++show x) $ zip (V.toList vf) (V.toList vasd)
      _ -> do let (vf,vasd) = mapSpectrum sqrt $ gwOnesidedPSDWaveData dtFFT $ maxSegWaveData wav
              mapM_ (\(t,x) -> hPutStrLn stdout $ (show t)++" "++show x) $ zip (V.toList vf) (V.toList vasd)


maxSegWaveData w = do
  let lenlist = map (\x-> V.length (gwdata x)) w
      maxI = maxIndex lenlist
   in w !! maxI

maxIndex list = snd . maximum $ zip list [0 .. ]

