{-******************************************
  *     File Name: Lwtprint.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/01/27 14:31:12
  *******************************************-}

module HasKAL.ExternalUtils.Lwtprint
  (execLwtprint
  ) where


import System.Process -- readProcess
import Control.Monad -- forM

execLwtprint :: [String] -> String -> Int -> Int -> [String] -> IO [String]
execLwtprint glitchActiveLabels kwBasename kwGpsTime kwStride kwActiveLabels = do
   forM [0..(length glitchActiveLabels)-1] $ \lambda -> do
      let tempGps = kwGpsTime + kwStride 
      let filePath = kwBasename ++ (glitchActiveLabels !! lambda) ++ "-" ++ (take 4 $ show kwGpsTime) ++ "/" ++ kwBasename ++ (glitchActiveLabels !! lambda) ++ "-" ++ (show tempGps) ++ "-" ++ (show kwStride) ++ ".xml"
      let cmd_string = [ filePath ,("-c " ++ (unwords kwActiveLabels)) ]
      readProcess "lwtprint" cmd_string [] -- execute lwtprint
