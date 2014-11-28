{-******************************************
  *     File Name: rtPlot.hs
  *        Author: Takahiro Yamamoto
  * Last Modified: 2014/11/28 14:09:35
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

  rng <- newRngWithSeed (-1)
  noft <- CM.forM [1..fs*tSec] $ \idxI -> gslRanTdist rng 3.0

  let noff = gwspectrogram 0 nfft fs noft
  rtPlot3D noff

