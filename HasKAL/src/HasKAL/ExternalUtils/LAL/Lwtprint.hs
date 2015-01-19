{- |
Module      : HasKAL.ExternalUtils.LAL.Lwtprint
Description : This is documentation tests.
Copyright   : (c) WhoAmI, 2014
License     : ???
Maintainer  : hoge@hoge.com
Stability   : test
Portability : POSIX

LAL function
-}

module HasKAL.ExternalUtils.LAL.Lwtprint
  (execLwtprint
  ) where


import qualified System.Process as SP -- readProcess
import qualified Control.Monad as CM -- forM

-- | KleineWelle tabel:
--
-- * ifo 
--
-- * peak_time
--
-- * peak_time_ns
--
-- * start_time
--
-- * start_time_ns
--
-- * duration
--
-- * search
--
-- * central_freq
--
-- * channel
--
-- * amplitude
--
-- * snr
--
-- * confidence 
--
-- * chisq
--
-- * chisq_dof
--
-- * bandwidth
--
-- * event_id
--
-- * process_id
--
-- * table
execLwtprint :: [String] -- ^ channel list
             -> String -- ^ prefix name
             -> Integer -- ^ GPS Time [sec]
             -> Int -- ^ Stride [sec]
             -> [String] -- ^ kleineWelle table
             -> IO [String]
execLwtprint glitchActiveLabels kwBasename kwGpsTime kwStride kwActiveLabels = do
   CM.forM [0..(length glitchActiveLabels)-1] $ \lambda -> do
      let tempGps = kwGpsTime + (fromIntegral kwStride)
      let filePath = kwBasename ++ (glitchActiveLabels !! lambda) ++ "-" ++ (take 5 $ show kwGpsTime) ++ "/" ++ kwBasename ++ (glitchActiveLabels !! lambda) ++ "-" ++ (show tempGps) ++ "-" ++ (show kwStride) ++ ".xml"
      let cmd_string = [ filePath ,("-c " ++ (unwords kwActiveLabels)) ]
      SP.readProcess "lwtprint" cmd_string [] -- execute lwtprint
