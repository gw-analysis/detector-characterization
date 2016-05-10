
import Data.Int (Int32)
import HasKAL.DataBaseUtils.FrameFull.Function (kagraDataGPS)
import HasKAL.FrameUtils.FrameUtils (existChannel)
import HasKAL.TimeUtils.GPSfunction (getCurrentGps)
import System.Environment (getArgs)
import System.IO (stdout, hPutStrLn)

main = do
  channel <- getArgs >>= \args -> case (length args) of
    1 -> return (head args)
    _ -> error "Usage: existChannelNow channel"
  gpsstr <- getCurrentGps
  let gpsnum = read gpsstr :: Int32
      gps = gpsnum - 100
  kagraDataGPS gps >>= \maybefiles -> case maybefiles of
    Nothing -> error "no file found."
    Just files -> do 
      let fname = head files
      existChannel channel fname >>= \e -> case e of
        [] -> False
        x  -> True

