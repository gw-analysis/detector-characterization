-- ghc -O2 test.hs ./HasKAL/PlotUtils/HROOT/AppendFunction.cc -o test -lFrame

import System.Environment (getArgs)
import Data.Packed.Vector (dim, fromList)
import Control.Monad (liftM)

import FrameUtils
import HasKAL.PlotUtils.HROOT.PlotGraph

main = do
  args <- getArgs
  case length args of
   2 -> return ()
   _ -> error "Usage: test FLAG1 FLAG2\n   FLAG1 = 0 or 1\n   FLAG1 = 0 or 1 or 2"

  let fName = "/home/yamamoto/data/L-L1_LOSC_4_V1-933199872-4096.gwf"
  chName <- case last args of
             "0" -> return "L1:LOSC-STRAIN"
             "1" -> return "L1:LOSC-DQMASK"
             "2" -> return "L1:LOSC-INJMASK"

  mbUnit <- getFrVectUnitY fName chName
  case mbUnit of
   Just x -> putStrLn $ "Unit= "++x
   Nothing -> putStrLn "Nothing"

  case head args of
   "0" -> return ()
   "1" -> do
     mbDat <- readFrameV chName fName 
     mbFs <- getSamplingFrequency fName chName 
     (fs, dat) <- case (mbFs, mbDat) of
                   (Just x, Just y) -> return (x, y)
                   (_, _)           -> error "Can't read fs or data"
     
     let tvec = fromList [0..(fromIntegral $ dim dat -1)/fs]
     plotV Linear Line 1 BLUE ("x", "y") 0.05 "t" (chName++".png") ((0,0),(0,0)) (tvec, dat)

