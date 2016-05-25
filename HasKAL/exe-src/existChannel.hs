
import Data.Int (Int32)
import HasKAL.DataBaseUtils.FrameFull.Function (existChannel, kagraDataGPS)
import HasKAL.TimeUtils.GPSfunction (getCurrentGps)
import System.Environment (getArgs)
import System.IO (stdout, hPutStrLn)

main = do
  (channel,gpsstr) <- getArgs >>= \args -> case (length args) of
    2 -> return (head args, args!!1)
    _ -> error "Usage: existChannel channel gps"
  let gps = read gpsstr :: Int32
  kagraDataGPS gps >>= \maybefiles -> case maybefiles of
    Nothing -> error "no file found."
    Just files -> do 
      let fname = head files
          e = existChannel channel fname
      case e of
        [] -> putStrLn "no"
        x  -> putStrLn "yes"

