{- |
Module      : plotSpectrumFromFrame
Description : This is documentation tests.
Copyright   : (c) WhoAmI, 2014
License     : ???
Maintainer  : Takahiro Yamamoto %mail%
Stability   : test
Portability : POSIX

-}{-
  * Last Modified: 2015/07/10 22:56:55
-}

import qualified Data.Vector.Storable as V (length)

import HasKAL.FrameUtils.Function (readFrameV, addTimeVect)
import HasKAL.FrameUtils.FrameUtils (getSamplingFrequency)
import HasKAL.SpectrumUtils.SpectrumUtils (gwpsdV)
import HasKAL.PlotUtils.HROOT.PlotGraph

main :: IO ()
main = do
  let fname = "/data/kagra/xend/R0202/K-K1_R-1115186650-32.gwf" -- ファイル名
      chname = "K1:PEM-EX_ACC_NO2_X_FLOOR" -- チャンネル名

  fs <- getSamplingFrequency fname chname -- サンプリング周波数取得
  hoft <- readFrameV chname fname -- 時系列データ取得
  
  let hoff = gwpsdV hoft (V.length hoft) fs -- FFT
  plotXV LogXY Line 1 BLUE ("frequency [Hz]", "[/rHz]") 0.05 "title" ((0,0),(0,0)) hoff

  
