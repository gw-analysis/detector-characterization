{- |
Module      : main
Description : This is documentation tests.
Copyright   : (c) WhoAmI, 2014
License     : ???
Maintainer  : Takahiro Yamamoto %mail%
Stability   : test
Portability : POSIX

-}{-
  * Last Modified: 2015/07/12 22:13:56
-}

{-
Compile: make
Usage: main /data/kagra/xend/R0206/K-K1_R-1120509568-32.gwf
-}

import HasKAL.MonitorUtils.CoherenceMon.Function (coherenceMon)

import Control.Monad (liftM, forM)
import System.Environment (getArgs)
import HasKAL.FrameUtils.FrameUtils (getChannelList, getSamplingFrequency)
import HasKAL.FrameUtils.Function (readFrameV)
import HasKAL.PlotUtils.HROOT.PlotGraph

main = do
  args <- getArgs
  case (length args) of
   1 -> do
     let fname = head args
     chList <- liftM (map fst) $ getChannelList fname
     dats <- mapM (`readFrameV` fname) chList
     forM (zip3 [1..] chList dats) $ \(n1, ch1, dat1) -> do
       fs1 <- getSamplingFrequency fname ch1
       forM (zip3 [1..] chList dats) $ \(n2, ch2, dat2) -> do
         fs2 <- getSamplingFrequency fname ch2
         case (n1<n2, fs1==fs2) of
          (True, True) -> do
            let coh = coherenceMon (truncate fs1) fs1 dat1 dat2
            plotV LogX Line 1 BLUE ("x","y") 0.05
              (ch1++" vs "++ch2) (ch1++ch2++".png") ((0,0),(-0.05,1.05)) coh
          (_, _) -> return ()
   _ -> error "Usage: main filename"

