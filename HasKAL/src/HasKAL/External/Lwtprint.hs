{-******************************************
  *     File Name: Lwtprint.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/01/24 10:53:04
  *******************************************-}

module HasKAL.External.Lwtprint
  (execlwt
  ) where


import System.IO
import System.Cmd
import System.Process
import Control.Monad

execlwt :: [String] -> String -> Int -> Int -> [String] -> IO [String]
execlwt glitchActiveLabels kwBasename kwGpsTime kwStride kwActiveLabels = do
   forM [0..(length glitchActiveLabels)-1] $ \lambda -> do
      let tempGps = kwGpsTime + kwStride 
      let filePath = kwBasename ++ (glitchActiveLabels !! lambda) ++ "-" ++ (take 4 $ show kwGpsTime) ++ "/" ++ kwBasename ++ (glitchActiveLabels !! lambda) ++ "-" ++ (show tempGps) ++ "-" ++ (show kwStride) ++ ".xml"
      let cmd_string = [ filePath ,("-c " ++ (unwords kwActiveLabels)) ]
      readProcess "lwtprint" cmd_string [] -- execute lwtprint
