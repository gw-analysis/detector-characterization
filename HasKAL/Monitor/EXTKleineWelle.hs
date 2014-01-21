{-******************************************
  *     File Name: EXTKleineWelle.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/01/24 11:06:11
  *******************************************-}

module HasKAL.Monitor.EXTKleineWelle
  (extKleineWelle
   --,geneOptKW
   --,execKW
  ) where


import System.IO
import System.Cmd
import System.Process
import Control.Monad

import HasKAL.External.Lwtprint


extKleineWelle :: Int -> String -> Double -> Double -> Double -> Int -> [String] -> Int -> Int -> Int -> String -> String -> Int -> [String] -> IO [String]
extKleineWelle kwStride kwBasename kwTransientDuration kwSignificance kwThreshold kwDecimateFactor glitchActiveLabels kwUnowen_1 kwFsample kwUnowen_2 optFilePref listFile kwGpsTime kwActiveLabels = do
  geneOptKW kwStride kwBasename kwTransientDuration kwSignificance kwThreshold kwDecimateFactor kwUnowen_1 kwFsample kwUnowen_2 optFilePref glitchActiveLabels
  execKW optFilePref listFile glitchActiveLabels
  execlwt glitchActiveLabels kwBasename kwGpsTime kwStride kwActiveLabels


geneOptKW :: Int -> String -> Double -> Double -> Double -> Int  -> Int -> Int -> Int -> String -> [String] -> IO [()]
geneOptKW kwStride kwBasename kwTransientDuration kwSignificance kwThreshold kwDecimateFactor kwUnowen_1 kwFsample kwUnowen_2 optFilePref glitchActiveLabels = do
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


execKW :: String -> String -> [String] -> IO [()]
execKW optFilePref listFile glitchActiveLabels = do
  forM [0..(length glitchActiveLabels)-1] $ \lambda -> do
    let cmd_string = "kleineWelleM " ++ optFilePref ++ (glitchActiveLabels !! lambda) ++ " -inlist " ++ listFile
    putStrLn cmd_string
    system cmd_string -- execute kleineWelleM
  return [()]

