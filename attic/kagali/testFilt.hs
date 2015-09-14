{- |
Module      : testFilt
Description : This is documentation tests.
Copyright   : (c) WhoAmI, 2014
License     : ???
Maintainer  : Takahiro Yamamoto %mail%
Stability   : test
Portability : POSIX

-}{-
  * Last Modified: 2015/09/14 13:32:59
-}

import System.Environment (getArgs)
import qualified Data.Vector.Storable as V (mapM, fromList)

import HasKAL.ExternalUtils.GSL.RandomNumberDistributions
import HasKAL.ExternalUtils.GSL.RandomNumberGeneration
import HasKAL.PlotUtils.HROOT.PlotGraph

import KAGALIUtils

main = do
  args <- getArgs
  (fmin, fmax) <- case (length args)/=2 of
                      True -> error "Usage: testFilt fmin fmax"
                      False -> return $ (args!!0, args!!1)

  let fs = 2048
  let tvec = V.fromList [1/fs, 2/fs..4]
  rng <- newRngWithSeed (100)
  dat1 <- V.mapM (\x -> id $ gslRanGaussian rng 1.0) tvec
  
  let dat2 = butterBandPass dat1 fs (read fmin) (read fmax)
  oPlotV Linear Line 1 [] ("time","amplitude") 0.05 "title" ("testFilt_"++fmin++"_"++fmax++".png") ((0,0),(0,0)) [(tvec, dat2), (tvec, dat1)]

