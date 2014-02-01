{-******************************************
  *     File Name: EXTKleineWelle.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/01/27 14:31:27
  *******************************************-}

module HasKAL.MonitorUtils.EXTKleineWelle
  (execKleineWelle
   --,generateOptKW
   --,execKW
  ) where


import System.IO --openFile
import System.Process -- system
import Control.Monad -- forM

import HasKAL.ExternalUtils.Lwtprint as External


execKleineWelle :: Int -> String -> Double -> Double -> Double -> Int -> [String] -> Int -> Int -> Int -> String -> String -> Int -> [String] -> IO [String]
execKleineWelle kwStride kwBasename kwTransientDuration kwSignificance kwThreshold kwDecimateFactor glitchActiveLabels kwUnowen_1 kwFsample kwUnowen_2 optFilePref listFile kwGpsTime kwActiveLabels = do
  generateOptKW kwStride kwBasename kwTransientDuration kwSignificance kwThreshold kwDecimateFactor kwUnowen_1 kwFsample kwUnowen_2 optFilePref glitchActiveLabels
  coreKleineWelle optFilePref listFile glitchActiveLabels
  External.execLwtprint glitchActiveLabels kwBasename kwGpsTime kwStride kwActiveLabels


generateOptKW :: Int -> String -> Double -> Double -> Double -> Int  -> Int -> Int -> Int -> String -> [String] -> IO [()]
generateOptKW kwStride kwBasename kwTransientDuration kwSignificance kwThreshold kwDecimateFactor kwUnowen_1 kwFsample kwUnowen_2 optFilePref glitchActiveLabels = do
  {--  Set parameters for optM format  --}
  let s_stride = "stride " ++ (show kwStride)
  let s_transientDuration = "transientDuration " ++ (show kwTransientDuration)
  let s_significance = "significance " ++ (show kwSignificance)
  let s_threshold = "threshold " ++ (show kwThreshold)
  let s_decimateFactor = "decimateFactor " ++ (show kwDecimateFactor)

  forM [0..(length glitchActiveLabels)-1] $ \lambda -> do
    let s_basename = "basename " ++ kwBasename ++ (glitchActiveLabels !! lambda)
    let s_channel = "channel " ++ (glitchActiveLabels !! lambda) ++ " " ++ (show kwUnowen_1) ++ " " ++ (show kwFsample) ++ " " ++ (show kwUnowen_2)

    {--  Merge parameters  --}
    let kwInfo = [s_stride, s_basename, s_transientDuration, s_significance, s_threshold, s_decimateFactor, s_channel]

    {--  Output parameter as optM file  --}
    oFile <- openFile (optFilePref ++ (glitchActiveLabels !! lambda)) WriteMode
    hPutStrLn oFile (unlines kwInfo)
    hClose oFile


coreKleineWelle :: String -> String -> [String] -> IO [()]
coreKleineWelle  optFilePref listFile glitchActiveLabels = do
  forM [0..(length glitchActiveLabels)-1] $ \lambda -> do
    let cmd_string = "kleineWelleM " ++ optFilePref ++ (glitchActiveLabels !! lambda) ++ " -inlist " ++ listFile
    putStrLn ("kleineWelleM " ++ cmd_string)
    system cmd_string -- execute kleineWelleM
  return [()]

