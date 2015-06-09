{- |
Module      : plotSpectrogram
Description : This is documentation tests.
Copyright   : (c) WhoAmI, 2014
License     : ???
Maintainer  : Takahiro Yamamoto %mail%
Stability   : test
Portability : POSIX
GUI of Antenna Pattern
-}{-
  * Last Modified: 2015/06/09 14:43:27
-}

import HasKAL.FrameUtils.Function (readFrameV, addTimeVect)
import HasKAL.FrameUtils.FrameUtils (getSamplingFrequency)
import HasKAL.SpectrumUtils.SpectrumUtils (gwspectrogramV)
import HasKAL.PlotUtils.HROOT.PlotGraph3D

main :: IO ()
main = do
  let fname = "/data/kagra/xend/R0202/K-K1_R-1115186650-32.gwf" -- ファイル名
      chname = "K1:PEM-EX_ACC_NO2_X_FLOOR" -- チャンネル名

  fs <- getSamplingFrequency fname chname -- サンプリング周波数取得
  hoft <- readFrameV chname fname -- 時系列データ取得
  
  let hoff = gwspectrogramV 0 (truncate fs) fs hoft -- SFT

  spectrogramMX LogYZ COLZ "[/rHz]" "title" hoff

