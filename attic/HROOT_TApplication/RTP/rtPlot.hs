{-******************************************
  *     File Name: rtPlot.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/11/28 15:33:04
  *******************************************-}

import Module

import Control.Monad as CM

import HasKAL.SpectrumUtils.SpectrumUtils as SU
import HasKAL.ExternalUtils.GSL.RandomNumberGeneration as RNG
import HasKAL.ExternalUtils.GSL.RandomNumberDistributions as RND


main = do
  let nfft = 256
      fs = 1024
      tSec = 64
      dt = 1.0/fs

  rng <- newRngWithSeed (-1)
  noft <- CM.forM [1..fs*tSec] $ \idxI -> gslRanTdist rng 3.0

  rtPlot $ zip [0.0, dt..] noft
  rtPlot3D $ gwspectrogram 0 nfft fs noft


