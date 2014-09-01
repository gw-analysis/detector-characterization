{-******************************************
  *     File Name: plot.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/08/30 20:26:58
  *******************************************-}

import HasKAL.PlotUtils.HROOT.PlotGraph as PG
import HasKAL.PlotUtils.HROOT.PlotGraph3D as PG3
import HasKAL.Misc.StrictMapping as MSM

main = do

  -- dats <- MSM.forM' filelist $ \filename -> do
  --   dat <- readFile filename
  --   return $ map l2t $ readMultiColumn dat

  -- PG3.spectrogram PG3.Linear PG3.COLZ "nu" ("SRMon") "X11" $ concat dats

  dat <- readFile file
  PG.plotX PG.LogXY PG.Line ("[Hz]","[V/rHz]") "Spectrum" $ map l2t' $ readMultiColumn dat

filelist :: [String]
filelist = ["./result1034554112.txt",
            "./result1034554496.txt",
            "./result1034554880.txt",
            "./result1034555264.txt",
            "./result1034555648.txt",
            "./result1034556032.txt"]

file = "./ave1034554496.txt"

l2t :: [Double] -> (Double, Double, Double)
l2t [] = (0.0, 0.0, 0.0)
l2t (x:[]) = (x, 0.0, 0.0)
l2t (x:y:[]) = (x, y, 0.0)
l2t (x:y:z:_) = (x, y, z)

l2t' :: [Double] -> (Double, Double)
l2t' [] = (0.0, 0.0)
l2t' (x:[]) = (x, 0.0)
l2t' (x:y:_) = (x, y)


readMultiColumn :: (Fractional a, Read a) => String -> [[a]]
readMultiColumn = map readOneColumn.lines

readOneColumn :: (Fractional a, Read a) => String -> [a]
readOneColumn = map read.words