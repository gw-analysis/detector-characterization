

import HasKAL.FrameUtils.FrameUtils (getChannelList)
import System.Environment (getArgs)
import System.IO (stdout, hPutStrLn)

main = do
  fname <- getArgs >>= \args -> case (length args) of
    1 -> return (head args)
    _ -> error "Usage: showChannels filename"
  getChannelList fname >>= \maybech -> case maybech of
    Nothing -> error "no channel in the file"
    Just x -> mapM_ (\(y, z)-> hPutStrLn stdout y) x

