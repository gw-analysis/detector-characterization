

import Control.Monad ((>>=))
import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import HasKAL.DataBaseUtils.Function (kagraChannelList,kagraDataGPS)
import System.Environment (getArgs)
import System.IO (stdout,  hPutStrLn)
 

main = do
  gps <- getArgs >>= \a-> case length a of
    1 -> return $ (read (head a) :: Int32)
    _ -> error "Usage: testkagraCHannelList gpstime"
  kagraChannelList gps >>= \maybech -> 
    mapM_ (hPutStrLn stdout) $ fromMaybe (error "no channel") maybech
