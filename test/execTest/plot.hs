{-******************************************
  *     File Name: plot.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/09/06 18:39:42
  *******************************************-}

import HasKAL.PlotUtils.HROOT.PlotGraph3D as PG3
import HasKAL.Misc.StrictMapping as MSM

main = do
  
  MSM.forM' filelist $ \filename -> do
    dat <- readFile filename
    PG3.spectrogram PG3.LogY PG3.COLZ "nu" ("SRMon") (filename++".png") $ map l2t $ readMultiColumn dat

fpath = "/home/yamamoto/workspace/result/QUANT0.99/base842747904/"

filelist :: [String]
filelist = [--fpath++"spectrogram842743808.txt",
            fpath++"spectrogram842747904.txt",
            fpath++"spectrogram842752000.txt",
            fpath++"spectrogram842756096.txt",
            fpath++"spectrogram842760192.txt"]--,
--            fpath++"842764288.txt"]

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