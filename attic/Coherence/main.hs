{- |
Module      : main
Description : This is documentation tests.
Copyright   : (c) WhoAmI, 2014
License     : ???
Maintainer  : Takahiro Yamamoto %mail%
Stability   : test
Portability : POSIX

-}{-
  * Last Modified: 2015/07/25 11:46:49
-}

{-
Compile: make
Usage: main /data/kagra/xend/R0206/K-K1_R-1120509568-32.gwf
-}


import Data.Vector.Storable

import HasKAL.PlotUtils.HROOT.PlotGraph
import Function


main = do
  -- パラメタ
  let fs = 16384.0
      duration = 128.0
      nfft = truncate fs

  -- data生成
  let xv = fromList [0, 1/fs..duration-1/fs]
      yv = xv

  -- 計算
  let zv = coherenceMon' nfft fs xv yv
  plotV Linear Line 1 BLUE ("x", "y") 0.05 "t" "a.png" ((0,0),(-0.05,1.05)) zv

