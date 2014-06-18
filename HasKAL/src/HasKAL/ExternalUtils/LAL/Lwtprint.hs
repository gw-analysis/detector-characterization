{-******************************************
  *     File Name: Lwtprint.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/06/18 20:54:32
  *******************************************-}

module HasKAL.ExternalUtils.LAL.Lwtprint
  (execLwtprint
  ) where


import qualified System.Process as SP -- readProcess
import qualified Control.Monad as CM -- forM

execLwtprint :: [String] -> String -> Integer -> Int -> [String] -> IO [String]
execLwtprint glitchActiveLabels kwBasename kwGpsTime kwStride kwActiveLabels = do
   CM.forM [0..(length glitchActiveLabels)-1] $ \lambda -> do
      let tempGps = kwGpsTime + (fromIntegral kwStride)
      let filePath = kwBasename ++ (glitchActiveLabels !! lambda) ++ "-" ++ (take 5 $ show kwGpsTime) ++ "/" ++ kwBasename ++ (glitchActiveLabels !! lambda) ++ "-" ++ (show tempGps) ++ "-" ++ (show kwStride) ++ ".xml"
      let cmd_string = [ filePath ,("-c " ++ (unwords kwActiveLabels)) ]
      SP.readProcess "lwtprint" cmd_string [] -- execute lwtprint
