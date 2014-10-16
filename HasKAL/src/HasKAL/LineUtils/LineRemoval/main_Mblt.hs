{-*********************************
 *main_Mblt.hs
 *Created:2014/09/02
 *Author:Mitsuhiro Asano
 **********************************
Last Modified: 2014/10/06 16:42:34
-}

import HasKAL.LineUtils.LineRemoval.RngMedian as RM
import HasKAL.LineUtils.LineRemoval.Mblt as MB
import HasKAL.SpectrumUtils.SpectrumUtils as HSU
import System.IO  

---- main ----
main = do
     handle <- openFile "LIGOtest.dat" ReadMode  
     contents <- hGetContents handle
     let contentsList = lines contents
     	 input = map read contentsList
     	 fs = 2048
	 sizeFreq = truncate $ (fromIntegral $ length input)/2.0 + 1	::Int
	 dt = 1.0/fs
	 time = take (length input) [0,dt..]

	 finfo = [(17.5,0.02),(60.0,0.02),(347.7,0.01),(695,0.015),(1004.6,0.02)] 
	 nrng = 3
     	 output = MB.mblt input fs finfo nrng

	 frequency = map fst $ HSU.gwpsd input (length input) fs
         psdNonMbltNonSignal = map snd $ HSU.gwpsd input (length input) fs
	 psdMbltNonSignal = map snd $ HSU.gwpsd output (length output) fs

     handle2 <- openFile "test.txt" WriteMode
     mapM (hPutStrLn handle2) $ take sizeFreq $ zipWith (++) (map show frequency) $ zipWith (++) (repeat " ") (map show psdMbltNonSignal)
     hClose handle2

---- Output Comment ----
     print "input : LIGOtest.dat "
     print "output : test.txt "
