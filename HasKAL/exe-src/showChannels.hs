
import Data.Int (Int32)
import HasKAL.DataBaseUtils.FrameFull.Function (kagraDataGPS)
import HasKAL.FrameUtils.FrameUtils (getChannelList)
import System.Environment (getArgs)
import System.IO (stdout, hPutStrLn)

main = do
  gpsstr <- getArgs >>= \args -> case (length args) of
    1 -> return (head args)
    _ -> error "Usage: showChannels gps"

  kagraDataGPS (read gpsstr :: Int32) >>= \maybefiles -> case maybefiles of
    Nothing -> error "no file found."
    Just files -> do
      let fname = head files
      getChannelList fname >>= \maybech -> case maybech of
        Nothing -> error "no channel at present."
        Just x -> mapM_ (\(y, z)-> hPutStrLn stdout y) x


