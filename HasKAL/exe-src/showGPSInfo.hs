

import Control.Monad ((>>=))
import HasKAL.FrameUtils.FrameUtils (getGPSTime)
import System.Environment (getArgs)
import System.IO (stdout, hPutStrLn)


main = do
  fname <- getArgs >>= \args -> case length args of
    1 -> return $ head args
    _ -> error "Usage: showGPSInfo frameFileName"
  getGPSTime fname >>= \maybegps -> case maybegps of
    Nothing -> hPutStrLn stdout "No GPS information."
    Just (gpss,gpsn,dt) -> hPutStrLn stdout $ "GPS: "++(show gpss)++"."++(show gpsn)++" +"++(show dt)++"s"


